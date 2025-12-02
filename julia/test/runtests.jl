using Test
include("../src/DSA.jl")
using .DSA

@testset "DSA Tests" begin

@testset "Stack Tests" begin
    s = Stack{Int}()
    @test isempty(s)
    push!(s, 1)
    push!(s, 2)
    push!(s, 3)
    @test !isempty(s)
    @test size(s) == 3
    @test pop!(s) == 3
    @test peek(s) == 2
    @test size(s) == 2
end

@testset "Queue Tests" begin
    q = Queue{Int}()
    @test isempty(q)
    enqueue!(q, 1)
    enqueue!(q, 2)
    enqueue!(q, 3)
    @test !isempty(q)
    @test length(q) == 3
    @test dequeue!(q) == 1
    @test peek(q) == 2
    @test length(q) == 2
end

@testset "DynamicArray Tests" begin
    arr = DynamicArray{Int}()
    push!(arr, 1)
    push!(arr, 2)
    push!(arr, 3)
    @test arr[1] == 1
    @test arr[2] == 2
    arr[2] = 5
    @test arr[2] == 5
    @test pop!(arr) == 3
    @test length(arr) == 2
    insert!(arr, 2, 10)
    @test arr[2] == 10
    @test remove_at!(arr, 2) == 10
end

@testset "SinglyLinkedList Tests" begin
    list = SinglyLinkedList{Int}()
    append!(list, 2)
    prepend!(list, 1)
    append!(list, 3)
    @test list[1] == 1
    @test list[2] == 2
    @test list[3] == 3
    @test length(list) == 3
    @test remove_first!(list) == 1
    @test length(list) == 2
end

@testset "DoublyLinkedList Tests" begin
    list = DoublyLinkedList{Int}()
    append!(list, 2)
    prepend!(list, 1)
    append!(list, 3)
    @test list[1] == 1
    @test list[2] == 2
    @test list[3] == 3
    @test remove_first!(list) == 1
    @test remove_last!(list) == 3
    @test length(list) == 1
end

@testset "Deque Tests" begin
    d = Deque{Int}()
    push_back!(d, 2)
    push_front!(d, 1)
    push_back!(d, 3)
    @test peek_front(d) == 1
    @test peek_back(d) == 3
    @test pop_front!(d) == 1
    @test pop_back!(d) == 3
    @test length(d) == 1
end

@testset "HashTable Tests" begin
    ht = HashTable{String, Int}()
    ht["one"] = 1
    ht["two"] = 2
    @test ht["one"] == 1
    @test ht["two"] == 2
    @test contains(ht, "one")
    @test !contains(ht, "three")
    @test remove!(ht, "one")
    @test !contains(ht, "one")
end

@testset "BinarySearchTree Tests" begin
    tree = BinarySearchTree{Int}()
    insert!(tree, 5)
    insert!(tree, 3)
    insert!(tree, 7)
    insert!(tree, 1)
    insert!(tree, 9)
    @test contains(tree, 5)
    @test contains(tree, 3)
    @test !contains(tree, 6)
    @test find_min(tree) == 1
    @test find_max(tree) == 9
    @test inorder(tree) == [1, 3, 5, 7, 9]
    @test remove!(tree, 3)
    @test !contains(tree, 3)
end

@testset "MinHeap Tests" begin
    heap = MinHeap{Int}()
    insert!(heap, 5)
    insert!(heap, 3)
    insert!(heap, 7)
    insert!(heap, 1)
    @test peek(heap) == 1
    @test extract_min!(heap) == 1
    @test extract_min!(heap) == 3
    @test extract_min!(heap) == 5
    @test extract_min!(heap) == 7

    heap2 = heapify([5, 3, 7, 1, 9])
    @test peek(heap2) == 1
end

@testset "DisjointSet Tests" begin
    ds = DisjointSet{Int}()
    make_set!(ds, 1)
    make_set!(ds, 2)
    make_set!(ds, 3)
    make_set!(ds, 4)
    @test !connected(ds, 1, 2)
    union!(ds, 1, 2)
    @test connected(ds, 1, 2)
    union!(ds, 3, 4)
    @test connected(ds, 3, 4)
    @test !connected(ds, 1, 3)
    union!(ds, 2, 3)
    @test connected(ds, 1, 4)
end

@testset "BinarySearch Tests" begin
    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    @test binary_search(arr, 5) == 5
    @test binary_search(arr, 1) == 1
    @test binary_search(arr, 10) == 10
    @test binary_search(arr, 11) === nothing

    arr2 = [1, 2, 2, 2, 3, 4, 5]
    @test lower_bound(arr2, 2) == 2
    @test upper_bound(arr2, 2) == 5
end

@testset "InsertionSort Tests" begin
    arr = [5, 2, 8, 1, 9, 3]
    insertion_sort!(arr)
    @test arr == [1, 2, 3, 5, 8, 9]

    arr2 = [5, 2, 8, 1, 9, 3]
    insertion_sort_desc!(arr2)
    @test arr2 == [9, 8, 5, 3, 2, 1]
end

@testset "MergeSort Tests" begin
    arr = [5, 2, 8, 1, 9, 3, 7, 4, 6]
    merge_sort!(arr)
    @test arr == [1, 2, 3, 4, 5, 6, 7, 8, 9]

    arr2 = [5, 2, 8, 1, 9, 3, 7, 4, 6]
    merge_sort_iterative!(arr2)
    @test arr2 == [1, 2, 3, 4, 5, 6, 7, 8, 9]
end

@testset "QuickSort Tests" begin
    arr = [5, 2, 8, 1, 9, 3, 7, 4, 6]
    quick_sort!(arr)
    @test arr == [1, 2, 3, 4, 5, 6, 7, 8, 9]

    arr2 = [5, 2, 8, 1, 9]
    @test quick_select!(copy(arr2), 1) == 1
    @test quick_select!(copy(arr2), 5) == 9
end

@testset "Graph Tests" begin
    g = Graph{Int}(false)
    add_edge!(g, 1, 2)
    add_edge!(g, 1, 3)
    @test has_vertex(g, 1)
    @test has_vertex(g, 2)
    @test has_edge(g, 1, 2)
    @test has_edge(g, 2, 1)  # Undirected

    dg = Graph{Int}(true)
    add_edge!(dg, 1, 2)
    @test has_edge(dg, 1, 2)
    @test !has_edge(dg, 2, 1)  # Directed
end

@testset "BFS Tests" begin
    g = Graph{Int}(false)
    add_edge!(g, 1, 2)
    add_edge!(g, 1, 3)
    add_edge!(g, 2, 4)

    order = bfs_traverse(g, 1)
    @test order[1] == 1
    @test length(order) == 4

    path = shortest_path(g, 1, 4)
    @test path[1] == 1
    @test path[end] == 4
    @test length(path) == 3
end

@testset "DFS Tests" begin
    g = Graph{Int}(false)
    add_edge!(g, 1, 2)
    add_edge!(g, 1, 3)
    add_edge!(g, 2, 4)

    order = dfs_traverse(g, 1)
    @test order[1] == 1
    @test length(order) == 4

    # Cycle detection
    cyclic = Graph{Int}(true)
    add_edge!(cyclic, 1, 2)
    add_edge!(cyclic, 2, 3)
    add_edge!(cyclic, 3, 1)
    @test has_cycle(cyclic)

    acyclic = Graph{Int}(true)
    add_edge!(acyclic, 1, 2)
    add_edge!(acyclic, 2, 3)
    @test !has_cycle(acyclic)

    # Topological sort
    sorted = topological_sort(acyclic)
    @test length(sorted) == 3
    @test findfirst(==(1), sorted) < findfirst(==(2), sorted)
end

end  # DSA Tests

println("All tests passed!")
