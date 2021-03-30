use petgraph::graph::{Graph, NodeIndex};
use petgraph::visit::EdgeRef;
use petgraph::Outgoing;
use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::FromIterator;
use std::cmp;

use indexmap::IndexSet;

#[derive(Debug,PartialEq)]
enum EdgeType {
    Tree,
    Back,
    Forward,
    Cross
}

type Edge = (NodeIndex, NodeIndex);

struct DFSResult {
    parent: HashSet<NodeIndex>,
    t: i32,
    start_time: HashMap<NodeIndex, i32>,
    finish_time: HashMap<NodeIndex, i32>,
    edges: HashMap<Edge, EdgeType>
}

// https://courses.csail.mit.edu/6.006/fall11/rec/rec14.pdf
impl DFSResult {
    pub fn process<N, E>(graph: &Graph::<N, E>) -> DFSResult {
        let mut result = DFSResult{ parent: HashSet::new(), t: 0, start_time: HashMap::new(), finish_time: HashMap::new(), edges: HashMap::new() };
        for node in graph.node_indices() {
            if !result.parent.contains(&node) {
                DFSResult::visit(&graph, node, &mut result, None);
            }
        }
        result
    }

    fn visit<N, E>(graph: &Graph::<N, E>, v: NodeIndex, mut result: &mut DFSResult, parent: Option<NodeIndex>) {
        result.parent.insert(v);
        result.t += 1;
        result.start_time.insert(v, result.t);
        if let Some(index) = parent {
            result.edges.insert((index, v), EdgeType::Tree);
        }

        let mut edges = graph.neighbors(v).detach();
        while let Some(n) = edges.next_node(&graph) {
            if !result.parent.contains(&n) {
                DFSResult::visit(&graph, n, &mut result, Some(v));
            } else if !result.finish_time.contains_key(&n) {
                result.edges.insert((v, n), EdgeType::Back);
            } else if result.start_time[&v] < result.finish_time[&n] {
                result.edges.insert((v, n), EdgeType::Forward);
            } else {
                result.edges.insert((v, n), EdgeType::Cross);
            }
        }

        result.t += 1;
        result.finish_time.insert(v, result.t);
    }
}

pub fn find_back_edge_nodes<N, E>(graph: &Graph::<N, E>) -> HashSet<NodeIndex> {
    let mut result: HashSet<NodeIndex> = HashSet::new();
    let dfsresult = DFSResult::process(&graph);
    for (edge, e_type) in &dfsresult.edges {
        if *e_type == EdgeType::Back {
            result.insert(edge.1);
        }
    }
    result
}

pub fn create_subgraph_with_node_indices<N: Clone, E: Clone>(graph: &Graph::<N, E>, node_indices: &Vec<NodeIndex>) -> Graph::<N, E> {
    let mut subgraph = Graph::<N, E>::new();
    let mut node_index_map: HashMap<NodeIndex, NodeIndex> = HashMap::new();
    for node in graph.node_indices() {
        //if !node_indices.iter().any(|&i| i == node) { continue; }
        let new_node = subgraph.add_node(graph.node_weight(node).unwrap().clone());
        node_index_map.insert(node, new_node);
    }
    for edge in graph.edge_references() {
        if !node_indices.iter().any(|&i| i == edge.source()) { continue; }
        if !node_indices.iter().any(|&i| i == edge.target()) { continue; }
        subgraph.add_edge(node_index_map[&edge.source()], node_index_map[&edge.target()], edge.weight().clone());
    }
    subgraph
}

type Blocked = IndexSet<NodeIndex>;
type BlockSet = HashMap<NodeIndex, Blocked>;
type Stack = VecDeque<NodeIndex>;

fn circuit<N, E>(graph: &Graph::<N, E>, subgraph: &HashSet<NodeIndex>, johnson: &mut Johnson, start_index: NodeIndex, vertex: NodeIndex) -> bool {
    let mut f = false; // whether a cycle was found
    johnson.stack.push_back(vertex);
    johnson.blocked.insert(vertex);

    for e in graph.edges_directed(vertex, Outgoing) {
        if !subgraph.contains(&e.source()) { continue; }
        if !subgraph.contains(&e.target()) { continue; }

        let successor = e.target();
        if successor == start_index {
            johnson.result.push(johnson.stack.iter().map(|x| *x).collect());
            f = true;
        } else if !johnson.blocked.contains(&successor) {
            if circuit(graph, subgraph, johnson, start_index, successor) {
                f = true;
            }
        }
    }
    if f {
        johnson.unblock(vertex);
    } else {
        for e in graph.edges_directed(vertex, Outgoing) {
            if !subgraph.contains(&e.source()) { continue; }
            if !subgraph.contains(&e.target()) { continue; }

            let w = e.target();
            match johnson.bset.get_mut(&w) {
                Some(k) => {
                    k.insert(vertex);
                },
                None => {
                    let mut k: Blocked = Blocked::with_capacity(1);
                    k.insert(vertex);
                    johnson.bset.insert(w, k);
                }
            }
        }
    }

    johnson.stack.pop_back();
    f
}

/*
 * Implementation of Tarjan's strongly connected components algorithm.
 * https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
 */
struct TarjanSCC<'a, N, E> {
    graph: &'a Graph::<N, E>,
    index: i32,
    path: VecDeque<NodeIndex>,
    path_set: HashSet<NodeIndex>,
    v_index: HashMap<NodeIndex, i32>,
    v_low_link: HashMap<NodeIndex, i32>,
    sccs: Vec<Vec<NodeIndex>>,
}

impl<'a, N, E> TarjanSCC<'a, N, E> {
    fn new(graph: &'a Graph::<N, E>) -> Self {
        TarjanSCC{ graph, path: VecDeque::new(), path_set: HashSet::new(), v_index: HashMap::new(), v_low_link: HashMap::new(), index: 0, sccs: Vec::new() }
    }

    fn find_sccs(graph: &'a Graph::<N, E>, start_index: NodeIndex) -> Vec<Vec<NodeIndex>> {
        let mut tarjan = TarjanSCC::new(graph);
        for i in tarjan.graph.node_indices() {
            if i < start_index { continue; }

            if !tarjan.v_index.contains_key(&i) {
                tarjan.get_sccs(start_index, i);
            }
        }

        tarjan.sccs
    }

    fn get_sccs(&mut self, start_index: NodeIndex, v: NodeIndex) {
        self.v_index.insert(v, self.index);
        self.v_low_link.insert(v, self.index);
        self.index += 1;

        self.path.push_back(v);
        self.path_set.insert(v);

        for e in self.graph.edges_directed(v, Outgoing) {
            let successor = e.target();
            if successor < start_index { continue; }

            if !self.v_index.contains_key(&successor) {
                self.get_sccs(start_index, successor);
                self.v_low_link.insert(v, cmp::min(*self.v_low_link.get(&v).unwrap(), *self.v_low_link.get(&successor).unwrap()));
            } else if self.path_set.contains(&successor) {
                self.v_low_link.insert(v, cmp::min(*self.v_low_link.get(&v).unwrap(), *self.v_index.get(&successor).unwrap()));
            }
        }

        if self.v_low_link.get(&v).unwrap() == self.v_index.get(&v).unwrap() {
            let mut result: Vec<NodeIndex> = Vec::new();
            let mut tmp: NodeIndex;
            loop {
                tmp = self.path.pop_back().unwrap();
                self.path_set.remove(&tmp);
                result.push(tmp);
                if tmp == v { break; }
            }
            if result.len() == 1 {
                let w = result.first().unwrap();
                if self.graph.contains_edge(v, *w) {
                    self.sccs.push(result);
                }
            } else {
                self.sccs.push(result);
            }
        }
    }
}

fn find_min_sccg<N, E>(graph: &Graph::<N, E>, start_index: NodeIndex) -> Option<(NodeIndex, Vec<NodeIndex>) > {
    let sccs = TarjanSCC::<N, E>::find_sccs(graph, start_index);

    let mut min_index_found = NodeIndex::new(usize::MAX);
    let mut min_scc: Option<Vec<NodeIndex>> = None;
    for scc in sccs {
        for v in &scc {
            if v < &min_index_found {
                min_index_found = *v;
                min_scc = Some(scc.clone());
            }
        }
    }

    if min_scc.is_none() {
        return None
    }

    Some((min_index_found, min_scc.unwrap()))
}

struct Johnson {
    blocked: Blocked,
    stack: Stack,
    bset: BlockSet,
    result: Vec<Vec<NodeIndex>>,
}

impl Johnson {
    fn new() -> Self {
        Johnson{ blocked: Blocked::new(), stack: Stack::new(), bset: BlockSet::new(), result: Vec::new() }
    }

    fn unblock(&mut self, u: NodeIndex) {
        self.blocked.remove(&u);
        if let Some(bu) = self.bset.get(&u) {
            let mut bu = bu.clone();
            self.bset.remove(&u);
            while !bu.is_empty() {
                let w = bu.pop().unwrap();
                if self.blocked.contains(&w) {
                    self.unblock(w);
                }
            }
        }
    }

}

/*
 * Implementation of "Finding all the elementary circuits of a directed graph",
 * Donald B. Johnson (SIAM J. Comput, Vol 4, No 1, March 1975)
 *
 * Implementation inspired by JGraphT's JohnsonSimpleCycles.java, written by
 * Nikolay Ognyanov and contributors.
 */
pub fn find_simple_cycles<N, E>(graph: &Graph::<N, E>) -> Vec<Vec<NodeIndex>> {
    let mut johnson = Johnson::new();

    let mut start_index: NodeIndex = NodeIndex::new(0);
    while start_index.index() < graph.node_count() {
        if let Some(sccg) = find_min_sccg(graph, start_index) {
            start_index = sccg.0;
            let scg = sccg.1;
            let subgraph: HashSet<NodeIndex> = HashSet::from_iter(scg.iter().map(|x| *x));

            let v = start_index;
            for e in graph.edges_directed(v, Outgoing) {
                let i = e.target();
                johnson.blocked.remove(&i);
                if let Some(bi) = johnson.bset.get_mut(&i) {
                    bi.clear();
                }
            }

            circuit(&graph, &subgraph, &mut johnson, start_index, start_index);
            start_index = NodeIndex::new(start_index.index() + 1);
        } else {
            break
        }
    }

    johnson.result
}

#[cfg(test)]
mod tests {
    use super::*;

    // Example of "How to write a basic control flow decompiler" by Warranty Voider
    fn create_warrantyvoider_graph() -> Graph::<usize, ()> {
        let mut graph = Graph::<usize, ()>::new();
        let n_0 = graph.add_node(0);
        let n_1 = graph.add_node(1);
        let n_2 = graph.add_node(2);
        let n_3 = graph.add_node(3);
        let n_4 = graph.add_node(4);
        let n_5 = graph.add_node(5);
        let n_6 = graph.add_node(6);
        let n_7 = graph.add_node(7);
        let n_8 = graph.add_node(8);
        let n_9 = graph.add_node(9);
        let n_10 = graph.add_node(10);
        let n_11 = graph.add_node(11);
        let n_12 = graph.add_node(12);
        let n_13 = graph.add_node(13);
        let n_14 = graph.add_node(14);
        let n_15 = graph.add_node(15);
        let n_16 = graph.add_node(16);
        let n_17 = graph.add_node(17);
        let n_18 = graph.add_node(18);
        let n_19 = graph.add_node(19);
        graph.add_edge(n_0, n_1, ());
        graph.add_edge(n_1, n_2, ());
        graph.add_edge(n_2, n_3, ());
        graph.add_edge(n_3, n_4, ());
        graph.add_edge(n_4, n_5, ());

        graph.add_edge(n_5, n_6, ());
        graph.add_edge(n_6, n_7, ());
        graph.add_edge(n_6, n_10, ());
        graph.add_edge(n_7, n_8, ());
        graph.add_edge(n_7, n_9, ());
        graph.add_edge(n_8, n_10, ());
        graph.add_edge(n_8, n_9, ());
        graph.add_edge(n_9, n_6, ());
        graph.add_edge(n_10, n_11, ());
        graph.add_edge(n_10, n_17, ());
        graph.add_edge(n_11, n_12, ());
        graph.add_edge(n_12, n_13, ());
        graph.add_edge(n_13, n_14, ());
        graph.add_edge(n_13, n_17, ());
        graph.add_edge(n_14, n_12, ());
        graph.add_edge(n_14, n_15, ());
        graph.add_edge(n_15, n_16, ());
        graph.add_edge(n_15, n_17, ());
        graph.add_edge(n_16, n_12, ());
        graph.add_edge(n_16, n_17, ());
        graph.add_edge(n_17, n_18, ());
        graph.add_edge(n_17, n_19, ());
        graph.add_edge(n_18, n_19, ());
        graph.add_edge(n_18, n_5, ());
        graph
    }

    // Example of "Johnson's Algorithm" by Tushar Roy,
    // https://www.youtube.com/watch?v=johyrWospv0
    fn create_tushar_graph() -> Graph::<usize, ()> {
        let mut graph = Graph::<usize, ()>::new();
        let _ = graph.add_node(0);
        let n_1 = graph.add_node(1);
        let n_2 = graph.add_node(2);
        let n_3 = graph.add_node(3);
        let n_4 = graph.add_node(4);
        let n_5 = graph.add_node(5);
        let n_6 = graph.add_node(6);
        let n_7 = graph.add_node(7);
        let n_8 = graph.add_node(8);
        let n_9 = graph.add_node(9);

        graph.add_edge(n_1, n_2, ());
        graph.add_edge(n_1, n_5, ());
        graph.add_edge(n_1, n_8, ());
        graph.add_edge(n_2, n_3, ());
        graph.add_edge(n_2, n_7, ());
        graph.add_edge(n_2, n_9, ());
        graph.add_edge(n_3, n_1, ());
        graph.add_edge(n_3, n_2, ());
        graph.add_edge(n_3, n_4, ());
        graph.add_edge(n_3, n_6, ());
        graph.add_edge(n_4, n_5, ());
        graph.add_edge(n_5, n_2, ());
        graph.add_edge(n_6, n_4, ());
        graph.add_edge(n_8, n_9, ());
        graph.add_edge(n_9, n_8, ());
        graph
    }

    fn vec_to_vec_of_nodeindex(input: &Vec<usize>) -> Vec<NodeIndex> {
        input.iter().map(|x| NodeIndex::new(*x)).collect()
    }

    fn vec_to_hashset(input: &Vec<usize>) -> HashSet<NodeIndex> {
        input.iter().map(|x| NodeIndex::new(*x)).collect()
    }

    fn find_simple_cycles_and_compare_results(graph: &Graph::<usize, ()>, expected: Vec<Vec<usize>>) {
        let cycles = find_simple_cycles(&graph);
        assert_eq!(expected.len(), cycles.len());
        for (n, expect) in expected.iter().enumerate() {
            let values = vec_to_vec_of_nodeindex(expect);
            assert_eq!(values, cycles[n]);
        }
    }

    #[test]
    fn find_back_edges_warrenty_voider() {
        let graph = create_warrantyvoider_graph();
        let expected = vec_to_hashset(&vec![ 5, 6, 12 ]);

        let back_edges = find_back_edge_nodes(&graph);
        assert_eq!(expected, back_edges);
    }

    #[test]
    fn find_cycles_warrenty_voider() {
        let graph = create_warrantyvoider_graph();
        let expected = vec![
            vec![ 5, 6, 10, 17, 18 ],
            vec![ 5, 6, 10, 11, 12, 13, 17, 18 ],
            vec![ 5, 6, 10, 11, 12, 13, 14, 15, 17, 18 ],
            vec![ 5, 6, 10, 11, 12, 13, 14, 15, 16, 17, 18 ],
            vec![ 5, 6, 7, 8, 10, 17, 18 ],
            vec![ 5, 6, 7, 8, 10, 11, 12, 13, 17, 18 ],
            vec![ 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18 ],
            vec![ 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18 ],
            vec![ 6, 7, 9 ],
            vec![ 6, 7, 8, 9 ],
            vec![ 12, 13, 14, 15, 16 ],
            vec![ 12, 13, 14 ],
        ];
        find_simple_cycles_and_compare_results(&graph, expected);
    }

    #[test]
    fn find_cycles_tushar() {
        let graph = create_tushar_graph();
        let expected = vec![
            vec![ 1, 5, 2, 3 ],
            vec![ 1, 2, 3 ],
            vec![ 2, 3, 6, 4, 5 ],
            vec![ 2, 3, 4, 5 ],
            vec![ 2, 3 ],
            vec![ 8, 9 ],
        ];
        find_simple_cycles_and_compare_results(&graph, expected);
    }
}
