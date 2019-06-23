#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a> {
    type Output;

    fn parse(&self, input: &'a str) -> ParseResult<'a, Self::Output>;
}

pub type OutTy<'a, P> = <P as Parser<'a>>::Output;

impl<'a, F, Output> Parser<'a> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    type Output = Output;

    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> Map<P, F>
where
    P: Parser<'a, Output=A>,
    F: Fn(A) -> B,
{
    Map { parser, map_fn }
}

struct Map<P, F> { parser: P, map_fn: F }
impl<'a, P, F, A, B> Parser<'a> for Map<P, F>
where
    P: Parser<'a, Output=A>,
    F: Fn(A) -> B,
{
    type Output = B;

    fn parse(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        let Map { parser, map_fn } = self;
        parser.parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> Pair<P1, P2>
where
    P1: Parser<'a, Output=R1>,
    P2: Parser<'a, Output=R2>,
{
    Pair { parser1, parser2 }
}

struct Pair<P1, P2> { parser1: P1, parser2: P2 }
impl<'a, P1, P2, R1, R2> Parser<'a> for Pair<P1, P2>
where
    P1: Parser<'a, Output=R1>,
    P2: Parser<'a, Output=R2>,
{
    type Output = (R1, R2);

    fn parse(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        let Pair { parser1, parser2 } = self;
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2.parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

type Left<'a, P1, P2> = Map<Pair<P1, P2>, fn((OutTy<'a, P1>, OutTy<'a, P2>)) -> OutTy<'a, P1>>;
fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> Left<'a, P1, P2>
where
    P1: Parser<'a, Output=R1>,
    P2: Parser<'a, Output=R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

type Right<'a, P1, P2> = Map<Pair<P1, P2>, fn((OutTy<'a, P1>, OutTy<'a, P2>)) -> OutTy<'a, P2>>;
fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> Right<'a, P1, P2>
where
    P1: Parser<'a, Output=R1>,
    P2: Parser<'a, Output=R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

type Identifier = fn(&str) -> ParseResult<'_, String>;
fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

fn match_literal(expected: &'static str) -> MatchLiteral {
    MatchLiteral { expected }
}

struct MatchLiteral { expected: &'static str }
impl<'a> Parser<'a> for MatchLiteral {
    type Output = ();

    fn parse(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        let MatchLiteral { expected } = *self;
        match input.get(0..expected.len()) {
            Some(next) if next == expected => Ok((&input[expected.len()..], ())),
            _ => Err(input),
        }
    }
}

fn one_or_more<'a, P, A>(parser: P) -> OneOrMore<P>
where
    P: Parser<'a, Output=A>,
{ OneOrMore { parser } }

struct OneOrMore<P> { parser: P }

impl<'a, P, A> Parser<'a> for OneOrMore<P>
where
    P: Parser<'a, Output=A>,
{
    type Output = Vec<A>;

    fn parse(&self, mut input: &'a str) -> ParseResult<'a, Self::Output> {
        let OneOrMore { parser } = self;

        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> ZeroOrMore<P>
where
    P: Parser<'a, Output=A>,
{ ZeroOrMore { parser } }

struct ZeroOrMore<P> { parser: P }

impl<'a, P, A> Parser<'a> for ZeroOrMore<P>
where
    P: Parser<'a, Output=A>,
{
    type Output = Vec<A>;

    fn parse(&self, mut input: &'a str) -> ParseResult<'a, Self::Output> {
        let ZeroOrMore { parser } = self;

        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

type AnyChar = fn(&str) -> ParseResult<'_, char>;
fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> Pred<P, F>
where
    P: Parser<'a, Output=A>,
    F: Fn(&A) -> bool,
{ Pred { parser, predicate } }

struct Pred<P, F> { parser: P, predicate: F }
impl<'a, P, A, F> Parser<'a> for Pred<P, F>
where
    P: Parser<'a, Output=A>,
    F: Fn(&A) -> bool,
{
    type Output = A;

    fn parse(&self, mut input: &'a str) -> ParseResult<'a, Self::Output> {
        let Pred { parser, predicate } = self;

        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

type WhitespaceChar = Pred<AnyChar, fn(&char) -> bool>;
fn whitespace_char() -> WhitespaceChar {
    pred(any_char, |c| c.is_whitespace())
}

type Space1 = OneOrMore<WhitespaceChar>;
fn space1() -> Space1 {
    one_or_more(whitespace_char())
}

type Space0 = ZeroOrMore<WhitespaceChar>;
fn space0() -> Space0 {
    zero_or_more(whitespace_char())
}

type QuotedString<'a> =
    Map<
        Right<'a,
            MatchLiteral,
            Left<'a,
                ZeroOrMore<
                    Pred<
                        AnyChar,
                        fn(&char) -> bool,
                    >,
                >,
                MatchLiteral,
        >>,
        fn(Vec<char>) -> String,
    >;

fn quoted_string<'a>() -> QuotedString<'a> {
    map(
        right(
            match_literal("\""),
            left(
                zero_or_more(pred(any_char, |c| *c != '"')),
                match_literal("\""),
            ),
        ),
        |chars| chars.into_iter().collect(),
    )
}

type AttributePair<'a> = Pair<Identifier, Right<'a, MatchLiteral, QuotedString<'a>>>;
fn attribute_pair<'a>() -> AttributePair<'a> {
    pair(identifier, right(match_literal("="), quoted_string()))
}

type Attributes<'a> = ZeroOrMore<Right<'a, Space1, AttributePair<'a>>>;
fn attributes<'a>() -> Attributes<'a> {
    zero_or_more(right(space1(), attribute_pair()))
}

type ElementStart<'a> = Right<'a, MatchLiteral, Pair<Identifier, Attributes<'a>>>;
fn element_start<'a>() -> ElementStart<'a> {
    right(match_literal("<"), pair(identifier, attributes()))
}

pub fn single_element<'a>() -> impl Parser<'a, Output=Element> {
    map(
        left(element_start(), match_literal("/>")),
        |(name, attributes)| Element {
            name,
            attributes,
            children: vec![],
        },
    )
}

#[test]
fn attribute_parser() {
    assert_eq!(
        Ok((
            "",
            vec![
                ("one".to_string(), "1".to_string()),
                ("two".to_string(), "2".to_string())
            ]
        )),
        attributes().parse(" one=\"1\" two=\"2\"")
    );
}

fn main() {
    println!("Hello, world!");
}
