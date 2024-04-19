use core::fmt;

use id_tree::Node;
use id_tree::NodeId;
use id_tree::InsertBehavior;
use id_tree_layout::Layouter;

const _AN: &str = "an";
const _SHOT: &str = "shot";
const _PAJAMAS: &str = "pajamas";
const _ELEPHANT: &str = "elephant";
const _I: &str = "I";
const _IN: &str = "in";
const _MY: &str = "my";

const BILL: &str = "Bill";
const PAST: &str = "[PAST]";
const SUDDENLY: &str = "suddently";
const HIT: &str = "hit";
const A: &str = "a";
const CAR: &str = "car";

struct DisplayNode {
    display_str: String,
}

impl id_tree_layout::Visualize for DisplayNode {
    fn visualize(&self) -> String {
        self.display_str.clone()
    }
}

#[derive(Debug, PartialEq)]
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
        Rule::Binary { prod: NonTerm::TP, one: NonTerm::NP, two: NonTerm::Tbar },
        Rule::Binary { prod: NonTerm::Tbar, one: NonTerm::T, two: NonTerm::VP },
        Rule::Binary { prod: NonTerm::VP, one: NonTerm::AdvP, two: NonTerm::Vbar },
        Rule::Binary { prod: NonTerm::Vbar, one: NonTerm::AdvP, two: NonTerm::Vbar },
        Rule::Binary { prod: NonTerm::Vbar, one: NonTerm::V, two: NonTerm::DP },
        Rule::Binary { prod: NonTerm::DP, one: NonTerm::D, two: NonTerm::NP },
        Rule::Binary { prod: NonTerm::Dbar, one: NonTerm::D, two: NonTerm::NP }
    ];

    let unit_rules = vec![
        Rule::Unit { prod: NonTerm::D, terminal: A },
        Rule::Unit { prod: NonTerm::V, terminal: HIT },
        Rule::Unit { prod: NonTerm::NP, terminal: BILL },
        Rule::Unit { prod: NonTerm::Nbar, terminal: BILL },
        Rule::Unit { prod: NonTerm::N, terminal: BILL },
        Rule::Unit { prod: NonTerm::NP, terminal: CAR },
        Rule::Unit { prod: NonTerm::Nbar, terminal: CAR },
        Rule::Unit { prod: NonTerm::N, terminal: CAR },
        Rule::Unit { prod: NonTerm::AdvP, terminal: SUDDENLY },
        Rule::Unit { prod: NonTerm::Advbar, terminal: SUDDENLY },
        Rule::Unit { prod: NonTerm::Adv, terminal: SUDDENLY },
        Rule::Unit { prod: NonTerm::T, terminal: PAST }
    ];

    let sent = [BILL, PAST, SUDDENLY, HIT, A, CAR];

    let mut chart : [[Vec<Tree>; 6]; 6] = Default::default();

    let n = 6;

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
    
    let mut i = 0;
    for parse_tree in &chart[0][n-1] {
        if let Tree::Node { root, ltree, rtree } = parse_tree {
            if !(**root == NonTerm::TP) {
                return
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

    /* if contains(&NonTerm::TP, &chart[0][n-1]) {
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
    if let Tree::Node { root, ltree, rtree } = tree {
        let root_id = d_tree.insert(Node::new(
                DisplayNode { display_str: root.to_string() }),
            InsertBehavior::UnderNode(parent_id)).unwrap();

        gen_display(d_tree, ltree, &root_id);
        gen_display(d_tree, rtree, &root_id);
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
