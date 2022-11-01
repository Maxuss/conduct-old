use crate::ast::Statement;

pub fn to_binary(code: Vec<Statement>) -> postcard::Result<Vec<u8>> {
    let mut out = Vec::new();
    out.extend(postcard::to_allocvec(&code)?);
    Ok(out)
}

pub fn from_binary(bin: &[u8]) -> postcard::Result<Vec<Statement>> {
    postcard::from_bytes(bin)
}
