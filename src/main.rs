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

impl<'a, F, Output> Parser<'a> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    type Output = Output;

    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, Output=B>
where
    P: Parser<'a, Output=A>,
    F: Fn(A) -> B,
{
    move |input|
        parser.parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, Output=(R1, R2)>
where
    P1: Parser<'a, Output=R1>,
    P2: Parser<'a, Output=R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2.parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, Output=R1>
where
    P1: Parser<'a, Output=R1>,
    P2: Parser<'a, Output=R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, Output=R2>
where
    P1: Parser<'a, Output=R1>,
    P2: Parser<'a, Output=R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

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

fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, Output=()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}


fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Output=Vec<A>>
where
    P: Parser<'a, Output=A>,
{
    move |mut input| {
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

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Output=Vec<A>>
where
    P: Parser<'a, Output=A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, Output=A>
where
    P: Parser<'a, Output=A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, Output=char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() -> impl Parser<'a, Output=Vec<char>> {
    one_or_more(whitespace_char())
}

fn space0<'a>() -> impl Parser<'a, Output=Vec<char>> {
    zero_or_more(whitespace_char())
}

fn quoted_string<'a>() -> impl Parser<'a, Output=String> {
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

fn attribute_pair<'a>() -> impl Parser<'a, Output=(String, String)> {
    pair(identifier, right(match_literal("="), quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Output=Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

fn element_start<'a>() -> impl Parser<'a, Output=(String, Vec<(String, String)>)> {
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
