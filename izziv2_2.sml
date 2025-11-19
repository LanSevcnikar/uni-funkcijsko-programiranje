(* There are no cycles in the graphs *)

signature GRAPH =
sig
    type node = string
    type edge = node * node * int
    type graph = edge list

    val create_graph: edge list -> graph
    val get_neighbors: node -> graph -> (node * int) list

    val get_shortest_path: node -> node -> graph -> int
    val get_number_of_paths: node -> node -> graph -> int
    val get_largest_capacity_path: node -> node -> graph -> int
    val get_most_reliable_path: node -> node -> graph -> int

end;

structure GraphImpl :> GRAPH =
struct
    type node = string
    type edge = node * node * int
    type graph = edge list

    fun create_graph edges = edges

    fun get_neighbors node graph =
        List.map (fn (_, dst, weight) => (dst, weight))
            (List.filter (fn (src, _, _) => src = node) graph)

    fun get_most_path src dst graph start_value worst_value aggregate_values aggregate_paths =
        let
            fun search curr =
                if curr = dst then 
                    start_value
                else
                    let
                        fun explore_neighbors N =
                            case N of
                                [] => worst_value
                                | (neighbor, weight)::rest => aggregate_paths ((explore_neighbors rest), (aggregate_values (weight, search neighbor)))

                    in
                        explore_neighbors (get_neighbors curr graph)
                    end
        in
            search src
        end

    fun get_shortest_path src dst graph =
        get_most_path src dst graph 0 1000 (fn (a, b) => a + b) (fn (a, b) => Int.min(a, b))

    fun get_number_of_paths src dst graph =
        get_most_path src dst graph 1 0 (fn (_, b) => b + 1) (fn (a, b) => a + b)

    fun get_largest_capacity_path src dst graph =
        get_most_path src dst graph 1000 ~1 (fn (a, b) => Int.min(a, b)) (fn (a, b) => Int.max(a, b))

    fun get_most_reliable_path src dst graph =
        get_most_path src dst graph 1 0 (fn (a, b) => a * b) (fn (a, b) => Int.max(a, b))

end

val graph_example = GraphImpl.create_graph
    [("A","B",3), ("B","C",1), ("A","D",2), ("B","D",4), ("C","D",5)];

val neighbors_of_A = GraphImpl.get_neighbors "A" graph_example;
val shortest_path_A_to_C = GraphImpl.get_shortest_path "A" "C" graph_example;
val shortest_path_A_to_D = GraphImpl.get_shortest_path "A" "D" graph_example;

val number_of_paths_A_to_D = GraphImpl.get_number_of_paths "A" "D" graph_example;
val largest_capacity_path_A_to_D = GraphImpl.get_largest_capacity_path "A" "D" graph_example;
val most_reliable_path_A_to_D = GraphImpl.get_most_reliable_path "A" "D" graph_example;