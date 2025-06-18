use crate::tokens::Token;

pub(crate) fn report_error(token: &Token, message: &str) {
    println!("{:?}: {}", token, message);
}
