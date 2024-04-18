use core::fmt;

const AN : &str = "an";
const SHOT : &str = "shot";
const PAJAMAS : &str = "pajamas";
const ELEPHANT : &str = "elephant";
const I : &str = "I";
const IN : &str = "in";
const MY : &str = "my";

#[derive(Debug, PartialEq)]
enum NonTerm {
    S,
    NP,
    VP,
    PP,
    In,
    Det,
    Vbd,
    Prp,
    Prps,
}

impl fmt::Display for NonTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
enum Rule {
    Binary {prod: NonTerm, one: NonTerm, two: NonTerm},
    Unit {prod: NonTerm, terminal: &'static str},
}

#[derive(PartialEq, Clone)]
enum Tree<'a> {
    Empty,
    Node {root: &'a NonTerm, ltree: Box<Tree<'a>>, rtree: Box<Tree<'a>>}
}

impl fmt::Display for Tree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "Empty"),
            Self::Node { root, .. } => write!(f, "{:?}", root),
        }
    }
}

impl fmt::Debug for Tree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "Empty"),
            Self::Node { root, .. } => write!(f, "{:?}", root),
        }
    }
}

fn main() {
    let rules = vec![
        Rule::Binary { prod: NonTerm::S, one: NonTerm::NP, two: NonTerm::VP },
        Rule::Binary { prod: NonTerm::PP, one: NonTerm::In, two: NonTerm::NP },
        Rule::Binary { prod: NonTerm::NP, one: NonTerm::Det, two: NonTerm::NP },
        Rule::Binary { prod: NonTerm::NP, one: NonTerm::NP, two: NonTerm::PP },
        Rule::Binary { prod: NonTerm::VP, one: NonTerm::Vbd, two: NonTerm::NP },
        Rule::Binary { prod: NonTerm::VP, one: NonTerm::VP, two: NonTerm::PP },
        Rule::Binary { prod: NonTerm::NP, one: NonTerm::Prps, two: NonTerm::NP }
    ];

    let unit_rules = vec![
        Rule::Unit { prod: NonTerm::Det, terminal: AN },
        Rule::Unit { prod: NonTerm::Vbd, terminal: SHOT },
        Rule::Unit { prod: NonTerm::NP, terminal: PAJAMAS },
        Rule::Unit { prod: NonTerm::NP, terminal: ELEPHANT },
        Rule::Unit { prod: NonTerm::NP, terminal: I },
        Rule::Unit { prod: NonTerm::Prp, terminal: I },
        Rule::Unit { prod: NonTerm::In, terminal: IN },
        Rule::Unit { prod: NonTerm::Prps, terminal: MY }
    ];

    let sent = [I, SHOT, AN, ELEPHANT, IN, MY, PAJAMAS];

    let mut chart : [[Vec<Tree>; 7]; 7] = Default::default();

    let n = 7;

    for i in 0..n {
        let word = sent[i];

        for rule in &unit_rules {
            if let Rule::Unit { prod, terminal } = rule {
                if &word == terminal {
                    let tree = Tree::Node {
                                root: prod,
                                ltree: Box::new(Tree::Empty),
                                rtree: Box::new(Tree::Empty)};
                    // hello("Found word for prod {:?}: {}", prod, word);
                    chart[i][i].push(tree);
                        
                }
            }
        }
    }

    for l in 2..=n {
        for i in 0..=(n - l) {
            let j = i + l - 1;
            // print!("({:?}, {:?}) ", i, j);

            for p in 1..=(l-1) {
                // chart[i][j-p] - left; chart[i+(l-p)][j] - down
                // print!("{:?} + {:?} ", chart[i][j-p], chart[i+p][j]);


                if chart[i][j-p].is_empty() || chart[i+(l-p)][j].is_empty() {
                    continue;
                }
                print!("(span {:?}) Non-empty prods: {:?} + {:?}: ", l, 
                    &chart[i][j-p], &chart[i+(l-p)][j]);

                for rule in &rules {
                    if let Rule::Binary { prod, one, two } = rule {

                        let mut valid_trees : Vec<Tree> = vec![];

                        // TODO: Fix this indent mess
                        for sub_term1 in &chart[i][j-p] {
                        // We have to insert a production for every instance of the NT
                            if let Tree::Node { root, .. } = *sub_term1 {
                                if root == one {
                                    for sub_term2 in &chart[i+(l-p)][j] {
                                        if let Tree::Node { root, .. } = *sub_term2 {
                                            if root == two {
                                                valid_trees.push(Tree::Node {
                                                    root: prod,
                                                    ltree: Box::new(sub_term1.clone()),
                                                    rtree: Box::new(sub_term2.clone()) 
                                                })
                                            }
                                        }
                                    }
                                }
                            }
                            
                        }

                        for v_tree in valid_trees {
                            print!("Adding production {:?} at ({:?}, {:?}); ",
                                   prod, i, j);

                            chart[i][j].push(v_tree);
                        }
                    }
                }
            
                println!();
            }
        }
        println!();
    }

    if !chart[0][n-1].is_empty() {
        let parse_tree = &chart[0][n-1][0];

        // build_display_tree(parse_tree);
    }

    /* if contains(&NonTerm::S, &chart[0][n-1]) {
        println!("Sentence belongs in the grammar");
    } */
}

fn _get_matches<'a>(target: &NonTerm, nterms: &'a Vec<&NonTerm>) -> Vec<&'a NonTerm> {
    let mut matches : Vec<&NonTerm> = vec![];

    for term in nterms {
        if target == *term {
            matches.push(*term);
        }
    }

    matches
}

fn node_count(tree: &Tree) -> usize {
    match tree {
        Tree::Node { root, ltree, rtree } =>
            1 + node_count(ltree) + node_count(rtree),

        Tree::Empty => 0,
    }
}

fn build_display_tree(tree: &Tree) {
    let n = node_count(tree);
    let mut d_tree : id_tree::Tree<String> = id_tree::TreeBuilder::new()
        .with_node_capacity(n).build();

    _gen_display(tree, &mut d_tree)
}

fn _gen_display(tree: &Tree, d_tree: &mut id_tree::Tree<String>) {
    match tree {
        Tree::Node { root, ltree, rtree } => {
            let root_id = d_tree.insert(id_tree::Node::new(root.to_string()),
                id_tree::InsertBehavior::AsRoot);
        },
        Tree::Empty => (),
    }
}

fn _get_root<'a>(tree: &'a Tree) -> Option<&'a NonTerm> {
    if let Tree::Node { root, .. } = tree {
        return Some(*root);
    }

    None
}

fn _contains(nterm: &NonTerm, nterms: &Vec<&NonTerm>) -> bool {
    for term in nterms {
        if *term == nterm {
            return true;
        }
    }

    false
}
