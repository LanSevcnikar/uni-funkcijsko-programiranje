(* There are no cycles in the graphs *)

signature GRAPH =
sig
    type node = string
    type edge = node * node * int
    type graph = edge list

    val create_graph: edge list -> graph
    val get_neighbors: node -> graph -> (node * int) list

    val get_shortest_path: node -> node -> graph -> (node list * int) option
    val get_number_of_paths: node -> node -> graph -> int
    val get_largest_capacity_path: node -> node -> graph -> (node list * int) option
    val get_most_reliable_path: node -> node -> graph -> (node list * int) option
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

    fun get_most_path src dst graph metric start_value comp =
        let
            fun keep_best_path paths =
                case paths of
                    [] => NONE
                  | (path, cost)::rest =>
                        let 
                            val best_in_rest = keep_best_path rest
                        in
                            case best_in_rest of
                                NONE => SOME (path, cost)
                              | SOME (best_path, best_cost) =>
                                    if comp (cost, best_cost) then SOME (path, cost)
                                    else SOME (best_path, best_cost)
                        end

            fun search curr =
                if curr = dst then 
                    SOME ([curr], start_value)
                else
                    let
                        val neighbors = get_neighbors curr graph

                        fun explore_neighbors [] = []
                          | explore_neighbors ((neighbor, weight)::rest) =
                                case search neighbor of
                                    NONE => explore_neighbors rest
                                  | SOME (path, cost) => 
                                        (curr::path, metric (cost, weight)) :: explore_neighbors rest

                        val paths = explore_neighbors neighbors

                        if List.null paths then NONE
                        else keep_best_path paths
                    end

        in
            case search src of
                NONE => NONE
              | SOME (path, cost) => SOME (path, cost)
        end

    fun get_shortest_path src dst graph =
        get_most_path src dst graph (fn (a, b) => a + b) 0 (fn (a, b) => a < b)

    fun get_largest_capacity_path src dst graph =
        get_most_path src dst graph (fn (a, b) => Int.min (a, b)) 10000 (fn (a, b) => a > b)

    fun get_most_reliable_path src dst graph =
        get_most_path src dst graph (fn (a, b) => a * b) 1 (fn (a, b) => a > b)

    fun get_number_of_paths src dst graph =
        let
        fun count_paths curr = 
            if curr = dst then 1
            else List.foldl (fn ((n, _), acc) => acc + count_paths n) 0 (get_neighbors curr graph)
        in
            count_paths src
        end

end

val graph_example = GraphImpl.create_graph
    [("A","B",3), ("B","C",1), ("A","D",2), ("B","D",4), ("C","D",5)];

val neighbors_of_A = GraphImpl.get_neighbors "A" graph_example;
val shortest_path_A_to_C = GraphImpl.get_shortest_path "A" "C" graph_example;
val shortest_path_A_to_D = GraphImpl.get_shortest_path "A" "D" graph_example;
val number_of_paths_A_to_D = GraphImpl.get_number_of_paths "A" "D" graph_example;
val largest_capacity_path_A_to_D = GraphImpl.get_largest_capacity_path "A" "D" graph_example;
val most_reliable_path_A_to_D = GraphImpl.get_most_reliable_path "A" "D" graph_example;