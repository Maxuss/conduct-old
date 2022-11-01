var multilineComment = /\/\*(?:[^*/]|\*(?!\/)|\/(?!\*)|<self>)*\*\//.source;
for (var i = 0; i < 2; i++) {
    // support 4 levels of nested comments
    multilineComment = multilineComment.replace(/<self>/g, function () { return multilineComment; });
}
multilineComment = multilineComment.replace(/<self>/g, function () { return /[^\s\S]/.source; });

Prism.languages.conduct = ({
    'comment': [
        {
            pattern: RegExp(/(^|[^\\])/.source + multilineComment),
            lookbehind: true,
            greedy: true
        },
        {
            pattern: /(^|[^\\:])\/\/.*/,
            lookbehind: true,
            greedy: true
        }
    ],
    'string': {
        pattern: /(^|[^\\])("|')(?:\\.|[^"\\\r\n])*("|')/,
        lookbehind: true,
        greedy: true
    },
    'function': {
        pattern: /((?:^|\s)fn[ \t]+)[a-zA-Z_]\w*(?=\s*\()/g,
        lookbehind: true
    },
    'keyword': /\b(?:fn|let|const|native|import|type|is|in|return|throw|if|else|bool|num|str)\b/,
    'boolean': /\b(?:false|nil|true)\b/,
    'class-name': Prism.languages.clike['class-name'],
    'constant': /\b[A-Z_][A-Z_\d]+\b/,
    'number': /\b(?:0x[\dA-Fa-f](?:_?[\dA-Fa-f])*|0o[0-7](?:_?[0-7])*|0b[01](?:_?[01])*|(?:(?:\d(?:_?\d)*)?\.)?\d(?:_?\d)*(?:[Ee][+-]?\d+)?)\b/,
    'boolean': /\b(false|true|nil)\b/,
    'operator': /[-+%=]=?|!=|:=|\*\*?=?|\/\/?=?|<[<=>]?|>[=>]?|[&|^~]/,
    'punctuation': /[{}[\];(),.:]/
})
