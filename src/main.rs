use core::fmt;
use std::env;
use std::error::Error;

use id_tree::Node;
use id_tree::NodeId;
use id_tree::InsertBehavior;
use id_tree_layout::Layouter;

use std::fs::File;
use std::path::Path;

struct DisplayNode {
    display_str: String,
}

impl id_tree_layout::Visualize for DisplayNode {
    fn visualize(&self) -> String {
        self.display_str.clone()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum NonTerm {
    TP,
    Tbar,
    T,
    NP,
    Nbar,
    N,
    VP,
    Vbar,
    V,
    AdvP,
    Advbar,
    Adv,
    DP,
    Dbar,
    D,
    PP,
    Pbar,
    P,
}

impl fmt::Display for NonTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Tbar => write!(f, "T'"),
            Self::Nbar => write!(f, "N'"),
            Self::Vbar => write!(f, "V'"),
            Self::Advbar => write!(f, "Adv'"),
            Self::Dbar => write!(f, "D'"),
            Self::Pbar => write!(f, "P'"),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug)]
enum Rule {
    Binary {prod: NonTerm, one: NonTerm, two: NonTerm},
    Unit {prod: NonTerm, terminal: String },
}

#[derive(PartialEq, Clone)]
enum Tree<'a> {
    Empty,
    Leaf(&'a str),
    Node {root: &'a NonTerm, ltree: Box<Tree<'a>>, rtree: Box<Tree<'a>>}
}

impl fmt::Display for Tree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "Empty"),
            Self::Leaf(s) => write!(f, "{}", s),
            Self::Node { root, .. } => write!(f, "{:?}", root),
        }
    }
}

impl fmt::Debug for Tree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "Empty"),
            Self::Leaf(s) => write!(f, "{}", s),
            Self::Node { root, .. } => write!(f, "{:?}", root),
        }
    }
}

fn read_sentence<P: AsRef<Path>>(filename: P) -> Result<Vec<(String, String)>, Box<dyn Error>> {
    let file = File::open(filename)?;
    let mut rdr = csv::ReaderBuilder::new()
        .has_headers(false)
        .from_reader(Box::new(file));
    
    let mut words: Vec<(String, String)> = vec![];

    for res in rdr.records() {
        let record = res?;

        if let (Some(s1), Some(s2)) = (record.get(0), record.get(1)) {
            words.push((s1.to_string(), s2.to_string()));
        } else {
            // Fix error handling; should just throw an error here
            println!("Improperly formed word");
        }
    }

    Ok(words)
}

// Try rewriting this where terminals are references to the words in words vector.
// words would need to be a reference for that
fn gen_unit_rules(words: Vec<(String, String)>) -> Vec<Rule> {
    
    let mut unit_rules: Vec<Rule> = vec![];

    for word in words {
        match (word.0, (word.1).as_str()) {
            (noun, "N") => {
                unit_rules.push(Rule::Unit { prod: NonTerm::NP, terminal: noun.clone() });
                unit_rules.push(Rule::Unit { prod: NonTerm::Nbar, terminal: noun.clone() });
                unit_rules.push(Rule::Unit { prod: NonTerm::N, terminal: noun.clone() });
            },
            (verb, "V") => {
                unit_rules.push(Rule::Unit { prod: NonTerm::V, terminal: verb.clone() });
            },
            (det, "D") => {
                unit_rules.push(Rule::Unit { prod: NonTerm::D, terminal: det.clone() });
            },
            (adv, "Adv") => {
                unit_rules.push(Rule::Unit { prod: NonTerm::AdvP, terminal: adv.clone() });
                unit_rules.push(Rule::Unit { prod: NonTerm::Advbar, terminal: adv.clone() });
                unit_rules.push(Rule::Unit { prod: NonTerm::Adv, terminal: adv.clone() });
            },
            (tense, "T") => {
                unit_rules.push(Rule::Unit { prod: NonTerm::T, terminal: tense.clone() });
            },
            (prop, "P") => {
                unit_rules.push(Rule::Unit { prod: NonTerm::P, terminal: prop.clone() });
            },
            _ => (),
        }
    }

    unit_rules
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <file_path>", &args[0]);
        return;
    }

    let file_path = &args[1];

    if let Err(e) = parse_cky(file_path) {
        println!("Error parsing sentence in .csv file: {:?}", e);
    }
}

fn parse_cky(filename: &str) -> Result<(), Box<dyn Error>> {
    let words = read_sentence(filename)?;
    let tagged_words = words.clone();

    let sent: Vec<&str> = (tagged_words).iter().map(
        |(word, _)| word.as_str()).collect();

    let new_unit_rules = gen_unit_rules(words);

    let rules = vec![
        Rule::Binary { prod: NonTerm::TP, one: NonTerm::NP, two: NonTerm::Tbar },
        Rule::Binary { prod: NonTerm::Tbar, one: NonTerm::T, two: NonTerm::VP },
        Rule::Binary { prod: NonTerm::NP, one: NonTerm::Nbar, two: NonTerm::PP },
        Rule::Binary { prod: NonTerm::Nbar, one: NonTerm::Nbar, two: NonTerm::PP },
        Rule::Binary { prod: NonTerm::VP, one: NonTerm::AdvP, two: NonTerm::Vbar },
        Rule::Binary { prod: NonTerm::Vbar, one: NonTerm::AdvP, two: NonTerm::Vbar },
        Rule::Binary { prod: NonTerm::VP, one: NonTerm::V, two: NonTerm::DP },
        Rule::Binary { prod: NonTerm::Vbar, one: NonTerm::V, two: NonTerm::DP },
        Rule::Binary { prod: NonTerm::VP, one: NonTerm::V, two: NonTerm::NP },
        Rule::Binary { prod: NonTerm::Vbar, one: NonTerm::V, two: NonTerm::NP },
        Rule::Binary { prod: NonTerm::VP, one: NonTerm::Vbar, two: NonTerm::PP },
        Rule::Binary { prod: NonTerm::Vbar, one: NonTerm::Vbar, two: NonTerm::PP },
        Rule::Binary { prod: NonTerm::DP, one: NonTerm::D, two: NonTerm::NP },
        Rule::Binary { prod: NonTerm::Dbar, one: NonTerm::D, two: NonTerm::NP },
        Rule::Binary { prod: NonTerm::PP, one: NonTerm::Pbar, two: NonTerm::PP },
        Rule::Binary { prod: NonTerm::Pbar, one: NonTerm::Pbar, two: NonTerm::PP },
        Rule::Binary { prod: NonTerm::PP, one: NonTerm::P, two: NonTerm::NP },
        Rule::Binary { prod: NonTerm::Pbar, one: NonTerm::P, two: NonTerm::NP },
        Rule::Binary { prod: NonTerm::PP, one: NonTerm::P, two: NonTerm::DP },
        Rule::Binary { prod: NonTerm::Pbar, one: NonTerm::P, two: NonTerm::DP }
    ];

    let n = sent.len();
    let mut chart: Vec<Vec<Vec<Tree>>> = vec![vec![vec![];n];n];

    for i in 0..n {
        let word = sent[i];

        for rule in &new_unit_rules {
            if let Rule::Unit { prod, terminal } = rule {
                if word == terminal {
                    let tree = Tree::Node {
                                root: prod,
                                ltree: Box::new(Tree::Leaf(terminal)),
                                rtree: Box::new(Tree::Empty)};
                    chart[i][i].push(tree);
                }
            }
        }
    }

    for l in 2..=n {
        for i in 0..=(n - l) {
            let j = i + l - 1;

            for p in 1..=(l-1) {
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
    
    let mut i = 0;
    for parse_tree in &chart[0][n-1] {
        if let Tree::Node { root, ltree: _, rtree: _ } = parse_tree {
            if !(**root == NonTerm::TP) {
                return Ok(())
            }

            if let Some(display_tree) = build_display_tree(parse_tree) {
                let path = format!("./parse_tree-{:?}.svg", i);
                let layouter = Layouter::new(&display_tree)
                    .with_file_path(std::path::Path::new(path.as_str()));
                layouter.write().expect("Failed creating parse tree image");

                i += 1;
            }    
        }
    }

    Ok(())
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
        Tree::Node { root: _, ltree, rtree } =>
            1 + node_count(ltree) + node_count(rtree),
        Tree::Leaf(_) => 1,
        Tree::Empty => 0,
    }
}

fn build_display_tree(tree: &Tree) -> Option<id_tree::Tree<DisplayNode>> {
    if let Tree::Node { root, ltree, rtree } = tree {
        let n = node_count(tree);
        let mut d_tree : id_tree::Tree<DisplayNode> = id_tree::TreeBuilder::new()
            .with_node_capacity(n).build();

        let root_id = d_tree.insert(Node::new(
                DisplayNode { display_str: root.to_string() }),
            id_tree::InsertBehavior::AsRoot).unwrap();

        // Question to ask? gen_display(&mut d_tree, &(*ltree), root_id);
        gen_display(&mut d_tree, ltree, &root_id);
        gen_display(&mut d_tree, rtree, &root_id);

        Some(d_tree)
    } else {
        None
    }
}

fn gen_display(d_tree: &mut id_tree::Tree<DisplayNode>, tree: &Tree,
    parent_id: &NodeId) {
    match tree {
        Tree::Node { root, ltree, rtree } => {
            let root_id = d_tree.insert(Node::new(
                DisplayNode { display_str: root.to_string() }),
                InsertBehavior::UnderNode(parent_id)).unwrap();

            let root_id = extend(d_tree, tree, root_id);

            gen_display(d_tree, ltree, &root_id);
            gen_display(d_tree, rtree, &root_id);
        },
        Tree::Leaf(s) => {
            let _ = d_tree.insert(Node::new(
                    DisplayNode { display_str: s.to_string() }),
                    InsertBehavior::UnderNode(parent_id));
        },
        _ => (),
    }
}

fn extend(d_tree: &mut id_tree::Tree<DisplayNode>, tree: &Tree,
    parent_id: NodeId) -> NodeId {
    if let Tree::Node { root, ltree, rtree } = tree {
        match (*root, (**ltree).clone(), (**rtree).clone()) {
            (NonTerm::NP, Tree::Leaf(_), Tree::Empty) => {
                let new_id = d_tree.insert(Node::new(
                    DisplayNode { display_str: NonTerm::Nbar.to_string() }),
                    // Question #2: why is this memory-safe? 
                    InsertBehavior::UnderNode(&parent_id)).unwrap();

                let new_id = d_tree.insert(Node::new(
                    DisplayNode { display_str: NonTerm::N.to_string() }),
                    InsertBehavior::UnderNode(&new_id)).unwrap();

                new_id
            },

            (NonTerm::Nbar, Tree::Leaf(_), Tree::Empty) => {
                let new_id = d_tree.insert(Node::new(
                    DisplayNode { display_str: NonTerm::N.to_string() }),
                    InsertBehavior::UnderNode(&parent_id)).unwrap();

                new_id
            },

            (NonTerm::VP, 
             Tree::Node { root: NonTerm::AdvP, ltree: _, rtree: _ },
             Tree::Node { root: NonTerm::Vbar, ltree: _, rtree: _ }) => {
                let new_id = d_tree.insert(Node::new(
                    DisplayNode { display_str: NonTerm::Vbar.to_string() }),
                    // Question #2: why is this memory-safe? 
                    InsertBehavior::UnderNode(&parent_id)).unwrap();

                new_id
            },

            (NonTerm::PP, 
             Tree::Node { root: NonTerm::P, ltree: _, rtree: _ },
             Tree::Node { root: _, ltree: _, rtree: _ }) => {
                let new_id = d_tree.insert(Node::new(
                    DisplayNode { display_str: NonTerm::Pbar.to_string() }),
                    // Question #2: why is this memory-safe? 
                    InsertBehavior::UnderNode(&parent_id)).unwrap();

                new_id
            },

            (NonTerm::AdvP, Tree::Leaf(_), Tree::Empty) => {
                let new_id = d_tree.insert(Node::new(
                    DisplayNode { display_str: NonTerm::Advbar.to_string() }),
                    InsertBehavior::UnderNode(&parent_id)).unwrap();

                let new_id = d_tree.insert(Node::new(
                    DisplayNode { display_str: NonTerm::Adv.to_string() }),
                    InsertBehavior::UnderNode(&new_id)).unwrap();

                new_id
            },

            (NonTerm::DP,
             Tree::Node { root: NonTerm::D, ltree: _, rtree: _ },
             Tree::Node { root: NonTerm::NP, ltree: _, rtree: _ }) => {
                let new_id = d_tree.insert(Node::new(
                    DisplayNode { display_str: NonTerm::Dbar.to_string() }),
                    InsertBehavior::UnderNode(&parent_id)).unwrap();

                new_id
            },
            
            (_, _, _) => parent_id,
        }
    } else {
        parent_id
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
