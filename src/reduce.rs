use crate::{code};

use std::collections::HashSet;

use petgraph::graph::EdgeReference;
use petgraph::graph::{EdgeIndex,NodeIndex};
use petgraph::visit::DfsPostOrder;
use petgraph::visit::EdgeRef;
use petgraph::{Direction, Incoming, Outgoing};

fn get_edge<'a>(graph: &'a code::CodeGraph, v: NodeIndex, dir: Direction, branch: code::Branch) -> Option<EdgeReference<'a, code::CodeEdge>> {
    let edges: Vec<EdgeReference<'a, code::CodeEdge>> = graph.edges_directed(v, dir).filter(|x| x.weight().branch == branch).collect();
    if edges.len() == 1 {
        return Some(*edges.first().unwrap());
    }
    None
}

fn get_outgoing_edge<'a>(graph: &'a code::CodeGraph, v: NodeIndex, branch: code::Branch) -> Option<EdgeReference<'a, code::CodeEdge>> {
    get_edge(graph, v, Outgoing, branch)
}


fn filter_outgoing_edges<'a>(graph: &'a code::CodeGraph, v: NodeIndex, branch: code::Branch) -> Vec<EdgeReference<'a, code::CodeEdge>> {
    graph.edges_directed(v, Outgoing).filter(|x| x.weight().branch == branch).collect()
}

fn has_outgoing_edge<'a>(graph: &'a code::CodeGraph, v: NodeIndex, branch: code::Branch) -> bool {
    !filter_outgoing_edges(graph, v, branch).is_empty()
}

fn reduce_graph_with<Func>(graph: &mut code::CodeGraph, func: Func) -> bool
where Func: Fn(&mut code::CodeGraph, NodeIndex) -> bool
{
    let mut visited_nodes: HashSet<NodeIndex> = HashSet::new();
    for n in graph.node_indices() {
        if visited_nodes.contains(&n) { continue }

        let mut dfs = DfsPostOrder::new(&*graph, n);
        while let Some(v) = dfs.next(&*graph) {
            visited_nodes.insert(v);
            if func(graph, v) {
                return true;
            }
        }
    }
    false
}

fn reduce_graph_if_else(graph: &mut code::CodeGraph) -> bool {
    reduce_graph_with(graph, |graph, n| {
        if has_outgoing_edge(graph, n, code::Branch::Always) { return false; }

        let true_branch = get_outgoing_edge(graph, n, code::Branch::True);
        if true_branch.is_none() { return false; }
        let true_branch = true_branch.unwrap();

        let false_branch = get_outgoing_edge(graph, n, code::Branch::False);
        if false_branch.is_none() { return false; }
        let false_branch = false_branch.unwrap();

        let true_target = true_branch.target();
        let false_target = false_branch.target();

        /*
         *             +-----n-----+
         * true_branch |           | false_branch
         *             |           |
         *             v           v
         * true_target o           o false_target
         *             |           |
         *   true_edge v           v false_edge
         *             +---dest----+
         *          true_dest/false_dest
         */
        let true_edge = get_outgoing_edge(&graph, true_target, code::Branch::Always);
        if true_edge.is_none() { return false; }
        let false_edge = get_outgoing_edge(&graph, false_target, code::Branch::Always);
        if false_edge.is_none() { return false; }

        let true_dest = true_edge.unwrap().target();
        let false_dest = false_edge.unwrap().target();
        if true_dest != false_dest { return false; } // not the same dest

        // Ensure there is only a single node to true_dest / false_dest
        if graph.edges_directed(true_target, Incoming).count() != 1 { return false; }
        if graph.edges_directed(false_target, Incoming).count() != 1 { return false; }

        let node = &graph[n];

        let ops = vec![ code::Operation::IfElse(node.ops.clone(), graph[true_target].ops.clone(), graph[false_target].ops.clone() ) ];
        let code_block = code::CodeNode{ script: node.script, ops };
        let new_node = graph.add_node(code_block);
        let nodes: Vec<(NodeIndex, code::CodeEdge)> = graph.edges_directed(n, Incoming).map(|e| (e.source(), e.weight().clone())).collect();
        for (index, edge) in nodes {
            graph.add_edge(index, new_node, edge);
        }
        graph.add_edge(new_node, true_dest, code::CodeEdge{ branch: code::Branch::Always });
        //println!("reduced if-else, node {:?}, invalidated {:?} {:?}, {:?}", graph[new_node].as_str(), graph[n].as_str(), graph[true_target].as_str(), graph[false_target].as_str());

        graph.remove_node(n);
        graph.remove_node(true_target);
        graph.remove_node(false_target);
        true
    })
}

fn reduce_graph_if(graph: &mut code::CodeGraph) -> bool {
    reduce_graph_with(graph, |graph, n| {
        if has_outgoing_edge(graph, n, code::Branch::Always) { return false; }

        let true_branch = get_outgoing_edge(graph, n, code::Branch::True);
        if true_branch.is_none() { return false; }
        let true_branch = true_branch.unwrap();

        let false_branch = get_outgoing_edge(graph, n, code::Branch::False);
        if false_branch.is_none() { return false; }
        let false_branch = false_branch.unwrap();

        let true_target = true_branch.target();
        let false_target = false_branch.target();

        let false_edge = get_outgoing_edge(&graph, false_target, code::Branch::Always);
        if false_edge.is_none() { return false; }

        // Only supported if for now is where the true-branch skips the extra work
        if true_target != false_edge.unwrap().target() { return false; }

        // TODO Need to check whether multiple nodes share targets here (like in if)

        /*
         *             +-----n-----+
         * true_branch |           | false_branch
         *             |           |
         *             |           v
         *             |           o false_target
         *             |           |
         *             v           v false_edge
         *             +----dest---+
         *          true_target/false_edge.target
         */

        let node = &graph[n];
        let ops = vec![ code::Operation::If(node.ops.clone(), graph[false_target].ops.clone()) ];
        let code_node = code::CodeNode{ script: node.script, ops };
        let new_node = graph.add_node(code_node);

        let nodes: Vec<(NodeIndex, code::CodeEdge)> = graph.edges_directed(n, Incoming).map(|e| (e.source(), e.weight().clone())).collect();
        for (index, edge) in nodes {
            graph.add_edge(index, new_node, edge);
        }
        graph.add_edge(new_node, true_target, code::CodeEdge{ branch: code::Branch::Always });

        //println!("reduced if, n {:?} false_target {:?}", &graph[n].as_str(), &graph[false_target].as_str());
        let true_branches: Vec<EdgeIndex> = filter_outgoing_edges(graph, n, code::Branch::True).iter().map(|b| b.id()).collect();
        for b in true_branches {
            graph.remove_edge(b);
        }
        graph.remove_node(n);
        graph.remove_node(false_target);
        true
    })
}

fn reduce_graph_sequential(graph: &mut code::CodeGraph) -> bool {
    reduce_graph_with(graph, |graph, n| {
        let n_outgoing: Vec<(NodeIndex, code::CodeEdge)> = graph.edges_directed(n, Outgoing).map(|e| (e.target(), e.weight().clone())).collect();
        if n_outgoing.len() != 1 { return false; }
        let n_outgoing = n_outgoing.first().unwrap();
        if n_outgoing.1.branch != code::Branch::Always { return false; }
        let target_index = n_outgoing.0;

        /*
         *      n
         *      |  n_outgoing
         *      |
         *      v
         *   target_index
         */

        let n_incoming: Vec<(NodeIndex, code::Branch)> = graph.edges_directed(n, Incoming).map(|e| (e.source(), e.weight().branch)).collect();
        if n_incoming.len() == 1 && n_incoming.first().unwrap().1 == code::Branch::Always {
            /*
             *   parent_node
             *      | n_incoming
             *      |
             *      v
             *      n
             *      |
             *      | n_outgoing
             *      v
             *   target_index
             */
            let mut n_ops = graph[n].ops.clone();
            let parent_index = n_incoming.first().unwrap().0;
            let parent_node = &mut graph[parent_index];

            parent_node.ops.append(&mut n_ops);
            //println!("reduce_graph_sequential (with parent): {:?}", &graph[n].as_str());
            graph.add_edge(parent_index, target_index, code::CodeEdge{ branch: code::Branch::Always });
            graph.remove_node(n);
            return true;
        } else if n_incoming.is_empty() {
            // n is root; we are going to remove it as that is easier (no edges to update)
            let mut n_ops = graph[n].ops.clone();
            let target_node = &mut graph[target_index];
            n_ops.append(&mut target_node.ops);
            target_node.ops = n_ops;
            //println!("reduce_graph_sequential (no parent): {:?} - removing {:?}", &graph[n].as_str(), &graph[target_index].as_str());

            graph.remove_node(n);
            return true;
        }
        false
    })
}

pub fn reduce_graph(graph: &mut code::CodeGraph) {
    let mut n: i32 = 0;
    loop {
        if false {
            println!(">>> reduce_graph iteration {}", n);
            //plot_graph(format!("dot/{}.dot", n).as_str(), graph);
            n += 1;
        }
        if reduce_graph_if_else(graph) { continue; }
        if reduce_graph_if(graph) { continue; }
        if reduce_graph_sequential(graph) { continue; }
        break;
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::{code, script, intermediate};

    // Graph must contain 2 nodes where node 0 -> 1 as an always branch
    fn verify_trivial_2_node_1_edge_graph(graph: &code::CodeGraph) {
        assert_eq!(graph.node_count(), 2);
        assert_eq!(graph.edge_count(), 1);
        let e0 = get_outgoing_edge(&graph, NodeIndex::new(0), code::Branch::Always);
        assert!(e0.is_some());
        let e0 = e0.unwrap();
        assert_eq!(e0.source(), NodeIndex::new(0));
        assert_eq!(e0.target(), NodeIndex::new(1));
    }

    // Graph must contain 1 node, no edges
    fn verify_trivial_1_node_no_edges_graph(graph: &code::CodeGraph) {
        assert_eq!(graph.node_count(), 1);
        assert_eq!(graph.edge_count(), 0);
    }

    fn make_code_frag(offset: u16) -> code::CodeFragment {
        let ops: Vec<intermediate::IntermediateCode> = Vec::new();
        let instructions = vec![ intermediate::Instruction{ offset, length: 1, ops } ];
        code::CodeFragment{ instructions }
    }

    #[test]
    fn reduce_if_simple() {
        /*
         *   0--+
         *  /   |       0
         * 1    | ===>  |
         *  \   |       1
         *   2--+
         */
        let script = script::ScriptBlock{ r#type: script::BlockType::Code, base: 0, data: &[] };
        let code_blocks = vec![
            code::CodeBlock{ script: &script, code: make_code_frag(0), branch_index_always: code::OffsetIndex::None, branch_index_true: code::OffsetIndex::Index(2), branch_index_false: code::OffsetIndex::Index(1) },
            code::CodeBlock{ script: &script, code: make_code_frag(1), branch_index_always: code::OffsetIndex::Index(2), branch_index_true: code::OffsetIndex::None, branch_index_false: code::OffsetIndex::None },
            code::CodeBlock{ script: &script, code: make_code_frag(2), branch_index_always: code::OffsetIndex::None, branch_index_true: code::OffsetIndex::None, branch_index_false: code::OffsetIndex::None },
        ];
        let mut graph = code::create_graph_from_codeblocks(&code_blocks);
        assert!(reduce_graph_if(&mut graph));
        assert!(!reduce_graph_if(&mut graph));
        verify_trivial_2_node_1_edge_graph(&graph);

        assert_eq!(graph[NodeIndex::new(0)].ops.len(), 1 as usize);
        //assert_eq!(graph[NodeIndex::new(0)].ops.first().unwrap(), Operation::If(
    }

    #[test]
    fn reduce_if_else_simple() {
        /*
         *   0
         *  / \        0
         * 1   2 ====> |
         *  \ /        1
         *   3
         */
        let script = script::ScriptBlock{ r#type: script::BlockType::Code, base: 0, data: &[] };
        let code_blocks = vec![
            code::CodeBlock{ script: &script, code: make_code_frag(0), branch_index_always: code::OffsetIndex::None, branch_index_true: code::OffsetIndex::Index(1), branch_index_false: code::OffsetIndex::Index(2) },
            code::CodeBlock{ script: &script, code: make_code_frag(1), branch_index_always: code::OffsetIndex::Index(3), branch_index_true: code::OffsetIndex::None, branch_index_false: code::OffsetIndex::None },
            code::CodeBlock{ script: &script, code: make_code_frag(2), branch_index_always: code::OffsetIndex::Index(3), branch_index_true: code::OffsetIndex::None, branch_index_false: code::OffsetIndex::None },
            code::CodeBlock{ script: &script, code: make_code_frag(3), branch_index_always: code::OffsetIndex::None, branch_index_true: code::OffsetIndex::None, branch_index_false: code::OffsetIndex::None },
        ];
        let mut graph = code::create_graph_from_codeblocks(&code_blocks);
        assert!(reduce_graph_if_else(&mut graph));
        assert!(!reduce_graph_if_else(&mut graph));
        verify_trivial_2_node_1_edge_graph(&graph);
    }

    #[test]
    fn reduce_sequential_simple_1() {
        /*
         * 0
         * |       0
         * 1 ====> |
         * |       1
         * 2
         */
        let script = script::ScriptBlock{ r#type: script::BlockType::Code, base: 0, data: &[] };
        let code_blocks = vec![
            code::CodeBlock{ script: &script, code: make_code_frag(0), branch_index_always: code::OffsetIndex::Index(1), branch_index_true: code::OffsetIndex::None, branch_index_false: code::OffsetIndex::None },
            code::CodeBlock{ script: &script, code: make_code_frag(1), branch_index_always: code::OffsetIndex::Index(2), branch_index_true: code::OffsetIndex::None, branch_index_false: code::OffsetIndex::None },
            code::CodeBlock{ script: &script, code: make_code_frag(2), branch_index_always: code::OffsetIndex::None, branch_index_true: code::OffsetIndex::None, branch_index_false: code::OffsetIndex::None },
        ];
        let mut graph = code::create_graph_from_codeblocks(&code_blocks);
        assert!(reduce_graph_sequential(&mut graph));
        verify_trivial_2_node_1_edge_graph(&graph);
    }

    #[test]
    fn reduce_sequential_simple_2() {
        /*
         * 0
         * | ====> 0
         * 1
         */
        let script = script::ScriptBlock{ r#type: script::BlockType::Code, base: 0, data: &[] };
        let code_blocks = vec![
            code::CodeBlock{ script: &script, code: make_code_frag(0), branch_index_always: code::OffsetIndex::Index(1), branch_index_true: code::OffsetIndex::None, branch_index_false: code::OffsetIndex::None },
            code::CodeBlock{ script: &script, code: make_code_frag(1), branch_index_always: code::OffsetIndex::None, branch_index_true: code::OffsetIndex::None, branch_index_false: code::OffsetIndex::None },
        ];
        let mut graph = code::create_graph_from_codeblocks(&code_blocks);
        assert!(reduce_graph_sequential(&mut graph));
        assert!(!reduce_graph_sequential(&mut graph));
        verify_trivial_1_node_no_edges_graph(&graph);
    }
}
