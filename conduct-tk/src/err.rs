use std::{collections::hash_map::Entry, fmt::Display, path::PathBuf};

use ariadne::{Cache, Color, Config, Fmt, Label, Report, Source, Span};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash, Eq)]
pub enum CodeSource {
    File(PathBuf),
    Builtin(PathBuf),
    Inline(String),
}

impl Display for CodeSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeSource::File(path) => write!(f, "file '{}'", path.to_str().unwrap()), // Error at line 12 in file 'src/io.cd'
            CodeSource::Inline(_) => write!(f, "inline source"), // Error at line 12 in inline source
            CodeSource::Builtin(name) => write!(f, "builtin module '{}'", name.as_path().display()), // Error at <unknown> in builtin module 'ct_util'
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct CodeArea {
    pub src: CodeSource,
    pub line: usize,
    pub span: (usize, usize),
}

impl Span for CodeArea {
    type SourceId = CodeSource;

    fn source(&self) -> &Self::SourceId {
        &self.src
    }

    fn start(&self) -> usize {
        self.span.0
    }

    fn end(&self) -> usize {
        self.span.1
    }
}

impl Display for CodeArea {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}-{} in {}",
            self.line, self.span.0, self.span.1, self.src
        )
    }
}

#[derive(Debug, Clone)]
pub enum ParsingError {
    UnexpectedEOF {
        at: CodeArea,
    },
    Unexpected {
        found: String,
        at: CodeArea,
    },
    Expected {
        expected: &'static str,
        found: String,
        at: CodeArea,
    },
    SyntaxError {
        message: String,
        at: CodeArea,
    },
    Handled,
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Handled => Ok(()),
            _ => {
                let report = self.to_owned().report().report();
                let mut buf = Vec::<u8>::with_capacity(1024); // preallocate string buffer
                report
                    .write(ConductCache::default(), &mut buf)
                    .map_err(|_| std::fmt::Error)?;
                write!(
                    f,
                    "{}",
                    String::from_utf8(buf).map_err(|_| std::fmt::Error)?
                )
            }
        }
    }
}

impl std::error::Error for ParsingError {}

impl ParsingError {
    pub fn report(self) -> ErrorReport {
        let mut colors = FancyColorGenerator::default();
        let a = colors.next();
        let b = colors.next();
        match self {
            ParsingError::UnexpectedEOF { at } => error(
                at.clone(),
                "Syntax Error",
                &[(at, &format!("Unexpected {}", "EOF".fg(a)))],
            ),
            ParsingError::Unexpected { found, at } => error(
                at.clone(),
                "Syntax Error",
                &[(at, &format!("Unexpected '{}'", found.fg(a)))],
            ),
            ParsingError::Expected {
                expected,
                found,
                at,
            } => error(
                at.clone(),
                "Syntax Error",
                &[(
                    at,
                    &format!("Expected {}, found {}", expected.fg(a), found.fg(b)),
                )],
            ),
            ParsingError::SyntaxError { message, at } => {
                error(at.clone(), "Syntax Error", &[(at, &message)])
            }
            Self::Handled => {
                unreachable!()
            }
        }
    }
}

fn error(area: CodeArea, message: &str, labels: &[(CodeArea, &str)]) -> ErrorReport {
    ErrorReport {
        call_stack: vec![],
        current_module: match &area.src {
            CodeSource::File(file) => file.display().to_string(),
            CodeSource::Builtin(builtin) => builtin.display().to_string(),
            CodeSource::Inline(_) => "none".to_string(),
        },
        position: area,
        message: message.to_owned(),
        labels: labels
            .into_iter()
            .map(|it| (it.0.clone(), it.1.to_owned()))
            .collect(),
    }
}

#[derive(Debug, Clone)]
pub struct ErrorReport {
    pub call_stack: Vec<CodeArea>,
    pub current_module: String,
    pub position: CodeArea,
    pub message: String,
    pub labels: Vec<(CodeArea, String)>,
}

impl ErrorReport {
    pub fn report(&self) -> Report<CodeArea> {
        let mut colors = FancyColorGenerator::default();

        let mut report = Report::build(
            ariadne::ReportKind::Error,
            self.position.src.clone(),
            self.position.span.0,
        )
        .with_config(Config::default().with_cross_gap(true))
        .with_message(self.message.clone());

        let mut i = 1;
        for area in &self.call_stack {
            let color = colors.next();
            report = report.with_label(
                Label::new(area.to_owned())
                    .with_order(i)
                    .with_message(&format!(
                        "{}: Error comes from here",
                        i.to_string().fg(color)
                    ))
                    .with_color(color)
                    .with_priority(1),
            );
            i += 1;
        }

        if self.labels.is_empty() || !self.labels.iter().any(|(a, _)| a == &self.position) {
            let color = colors.next();
            report = report.with_label(
                Label::new(self.position.clone())
                    .with_order(i)
                    .with_color(color)
                    .with_message(&self.message)
                    .with_priority(2),
            );
        }
        if i == 1 && self.labels.len() == 1 {
            let color = colors.next();
            report = report.with_label(
                Label::new(self.labels[0].0.clone())
                    .with_message(self.labels[0].1.clone())
                    .with_order(i)
                    .with_color(color)
                    .with_priority(2),
            );
        } else if !self.labels.is_empty() {
            for (area, label) in &self.labels {
                let color = colors.next();
                report = report.with_label(
                    Label::new(area.clone())
                        .with_message(&format!("{}: {}", i.to_string().fg(color), label))
                        .with_order(i)
                        .with_color(color)
                        .with_priority(2),
                );
                i += 1;
            }
        }
        report.finish()
    }
}

pub type Res<T> = Result<T, ParsingError>;

#[macro_export]
macro_rules! bail {
    ($pos:expr, $message:literal) => {
        return Err($crate::err::ParsingError::SyntaxError {
            message: $message.to_owned(),
            at: $pos,
        })
    };
}

#[macro_export]
macro_rules! check {
    ($err:expr) => {{
        match $err {
            Ok(v) => v,
            Err(e) => match e {
                ParsingError::Handled => return Err(e),
                _ => {
                    e.clone()
                        .report()
                        .report()
                        .print($crate::err::ConductCache::default())
                        .unwrap();
                    return Err(ParsingError::Handled);
                }
            },
        }
    }};
}

#[derive(Default)]
pub struct ConductCache {
    files: std::collections::HashMap<CodeSource, Source>,
}

impl Cache<CodeSource> for ConductCache {
    fn fetch(&mut self, id: &CodeSource) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        Ok(match self.files.entry(id.clone()) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => entry.insert(Source::from(match id {
                CodeSource::File(path) => {
                    std::fs::read_to_string(path).map_err(|e| Box::new(e) as _)?
                }
                CodeSource::Builtin(_) => "<builtin code>".to_owned(),
                CodeSource::Inline(inlined) => inlined.clone(),
            })),
        })
    }

    fn display<'a>(&self, id: &'a CodeSource) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(match id {
            CodeSource::File(path) | CodeSource::Builtin(path) => Box::new(path.display()),
            CodeSource::Inline(_) => Box::new("<inlined>"),
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FancyColorGenerator(f32, f32, f32);

impl Default for FancyColorGenerator {
    fn default() -> Self {
        Self(0.0, 1.5, 0.8)
    }
}

impl FancyColorGenerator {
    pub fn next(&mut self) -> Color {
        self.0 += 20.0;
        self.0 %= 360.0;

        let hsl = *self;

        let c = (1.0 - (hsl.2 * 2.0 - 1.0).abs()) * hsl.1;
        let h = hsl.0 / 60.0;
        let x = c * (1.0 - (h % 2.0 - 1.0).abs());
        let m = hsl.2 - c * 0.5;

        let (red, green, blue) = if h >= 0.0 && h < 0.0 {
            (c, x, 0.0)
        } else if (1.0..2.0).contains(&h) {
            (x, c, 0.0)
        } else if (2.0..3.0).contains(&h) {
            (0.0, c, x)
        } else if (3.0..4.0).contains(&h) {
            (0.0, x, c)
        } else if (4.0..5.0).contains(&h) {
            (x, 0.0, c)
        } else {
            (c, 0.0, x)
        };

        ariadne::Color::RGB(
            ((red + m) * 255.0) as u8,
            ((green + m) * 255.0) as u8,
            ((blue + m) * 255.0) as u8,
        )
    }
}
