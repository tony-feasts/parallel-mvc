# Parallel MVC

A brute-force approach to solving the **Minimum Vertex Cover (MVC)** problem in Haskell, using various parallel and memory-efficient strategies. This project demonstrates how different parallelization techniques and data structures can dramatically affect performance for NP-complete search problems.

---

## Table of Contents

1. [Overview](#overview)  
2. [Project Layout](#project-layout)  
3. [Installation](#installation)  
4. [Usage](#usage)  
    - [Generating Random Graphs](#generating-random-graphs)  
    - [Solving for MVC](#solving-for-mvc)  
    - [Running Tests](#running-tests)  
5. [Parallel Implementations](#parallel-implementations)  
6. [Citing This Work](#citing-this-work)  
7. [References](#references)  
8. [License](#license)  

---

## Overview

**Minimum Vertex Cover** is an NP-complete problem: given a graph \( G = (V, E) \), find the smallest subset of vertices \( V' \subseteq V \) such that every edge in \( E \) is incident to at least one vertex in \( V' \). This repository provides:

- **Multiple brute-force MVC solvers** in Haskell
- **Parallel** and **sequential** variations
- Techniques for **memory-efficient** subset generation
- A random **graph generator** to test algorithm performance
- Sample code to illustrate how concurrency impacts runtime and memory usage

You can switch between parallel algorithms (e.g., `ParallelV1`, `ParallelV2`, `ParallelV3`, `ParallelV4`) and sequential algorithms (`SequentialV1`, `SequentialV2`) to see how each handles subset generation, caching, and verification.

---

## Project Layout

```
parallel-mvc/
├── app/
│   ├── graph-generator/
│   │   └── Main.hs         # Generates and saves random graphs
│   └── mvc/
│       └── Main.hs         # Runs an MVC solver of your choice
│
├── src/
│   ├── GraphGenerator.hs    # Graph generation + adjacency list parsing
│   ├── ParallelV1.hs        # First parallel version (depth-based, chunk verify)
│   ├── ParallelV2.hs        # Cached subset generation approach
│   ├── ParallelV3.hs        # Top-down DFS w/ parallel depth control
│   ├── ParallelV4.hs        # Combination approach w/ combinatorial subsets
│   ├── SequentialV1.hs      # Naive DFS + subset generation
│   ├── SequentialV2.hs      # Deque-based subset generation
│   └── SubsetTools.hs       # Helpers for combinatorial indexing & generation
│
├── test/
│   ├── pv1.hs              # Test harness for ParallelV1
│   ├── pv2.hs              # Test harness for ParallelV2
│   ├── pv3.hs              # Test harness for ParallelV3
│   ├── pv4.hs              # Test harness for ParallelV4
│   ├── sv1.hs              # Test harness for SequentialV1
│   └── sv2.hs              # Test harness for SequentialV2
│
├── data/                    # Where generated graphs are stored
│   └── vN...eM....txt
│
├── package.yaml             # Project config for Stack
└── README.md                # You're reading it now!
```

Key highlights:

- **`GraphGenerator.hs`**: Functions to create random graphs and parse adjacency lists.  
- **`ParallelVx.hs`**: Four different parallel approaches, each refining memory usage and concurrency depth.  
- **`SequentialVx.hs`**: Two sequential versions illustrating different data structures.  
- **`SubsetTools.hs`**: Tools for direct combinatorial indexing and generating nth subsets.  

---

## Installation

You’ll need:

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) (recommended)  
- GHC (Glasgow Haskell Compiler)  
- A POSIX-like environment (macOS/Linux/WSL). Windows is also fine with Stack.  

**Steps**:

1. **Clone this repository**:
   ```bash
   git clone https://github.com/tony-feasts/parallel-mvc.git
   cd parallel-mvc
   ```
2. **Build the project** using Stack:
   ```bash
   stack build
   ```
   This will fetch all dependencies (e.g., `vector`, `random`, `monad-par`, `containers`) and compile everything.

---

## Usage

### Generating Random Graphs

Use the `graph-generator` executable to create a random graph with \( n \) vertices and \( m \) edges:

```bash
stack exec graph-generator -- <n> <m>
```

- **Example**: `stack exec graph-generator -- 20 190` creates a graph with 20 vertices and 190 edges.  
- The generated file will be in `data/` named `v<n>e<m>.txt`, e.g. `data/v20e190.txt`.

### Solving for MVC

Use the `mvc` executable to run any solver on an existing graph:

```bash
stack exec mvc -- <n> <m> <c>
```

1. **n** and **m** must match the generated file `data/v<n>e<m>.txt`.
2. **c** is often used as a concurrency-depth parameter or chunk size (depending on which parallel version you invoke in the code).  
3. Example:
   ```bash
   # Tweak your RTS options for parallelism (-N8) and bigger nursery (-A128M):
   stack exec mvc -- +RTS -N8 -A128M -RTS 20 190 10000
   ```
   This might run `ParallelV4` (depending on the code in `Main.hs`) with a chunk size of 10000.

> **Important**: The actual solver called depends on which import you have in `app/mvc/Main.hs`. By default, you will see `import ParallelV4 (solve)`, but feel free to switch to `ParallelV3`, `SequentialV1`, etc.

### Running Tests

This project includes multiple test targets (`pv1`, `pv2`, `pv3`, `pv4`, `sv1`, `sv2`) corresponding to each approach. For instance:

```bash
# Run ParallelV1's test harness with 8 cores
stack test :pv1 --ta "+RTS -l -N8 -A128M -RTS"
```

Most test scripts expect you to have a data file in `data/` (like `v20e190.txt`). Adjust `n`, `m`, or concurrency parameters in the `.hs` test files as needed.

---

## Parallel Implementations

1. **ParallelV1**:  
   - Recursively generates subsets in parallel to a specified depth.  
   - Chunks subsets into batches (size = 1000 by default) for parallel verification.  
   - High memory usage and garbage collection overhead.

2. **ParallelV2**:  
   - Uses a **Map-based cache** of subsets to avoid regenerating them multiple times.  
   - Faster subset generation but poor memory efficiency (enormous cache).  
   - Shows how caching can be a double-edged sword for GC.

3. **ParallelV3**:  
   - **Top-down DFS** merging generation and verification.  
   - Only small amounts of state are alive at once, leading to minimal GC.  
   - Depth-based parallelism with `par` / `pseq`.  
   - Fastest and most memory-efficient approach overall.

4. **ParallelV4**:  
   - Combinatorial indexing approach with a **mutable vector** and direct subsets (no large caches).  
   - Splits the combination space into ranges, processes each range in parallel.  
   - Very parallel-friendly but can be slower than V3 for smaller graphs due to overhead in factorial arithmetic.

---

## Citing This Work

If you use this code or reference the techniques illustrated here, feel free to cite the project report (in `\LaTeX{}`) located near the end of this README snippet or reference the repository:

- **GitHub**: [Parallel MVC by tony-feasts](https://github.com/tony-feasts/parallel-mvc)

---

## References

1. [VertexCoverSearch by sedgwickc](https://github.com/sedgwickc/VertexCoverSearch)  
2. [Haskell `vector` Package](https://hackage.haskell.org/package/vector)  
3. [Haskell `Data.IntSet`](https://hackage.haskell.org/package/containers-0.7/docs/Data-IntSet.html)  
4. [GHC Runtime Control](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/runtime_control.html)  
5. [Foldr vs Foldl vs Foldl'](https://wiki.haskell.org/Foldr_Foldl_Foldl')  
6. [Combinatorial Number System](https://en.wikipedia.org/wiki/Combinatorial_number_system)  
7. [Monad-par Library](https://hackage.haskell.org/package/monad-par)  
8. [Parallel Tuning in GHC](https://simonmar.github.io/publications/multicore-ghc.pdf)  
9. [System.IO.Unsafe](https://hackage.haskell.org/package/base-4.21.0.0/docs/System-IO-Unsafe.html)  
10. [Data.Char](https://hackage.haskell.org/package/base-4.20.0.0/candidate/docs/Data-Char.html)  
11. [Control.Monad.ST](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Monad-ST.html)  
12. [Data.IORef](https://hackage.haskell.org/package/base-4.20.0.0/candidate/docs/Data-IORef.html)  
13. [System.Random](https://hackage.haskell.org/package/random-1.2.1.3/docs/System-Random.html)  
14. [System.IO](https://hackage.haskell.org/package/base-4.21.0.0/docs/System-IO.html)  
15. [readMaybe](https://hoogle.haskell.org/?hoogle=readMaybe)  
16. [Parallel Tuning References](https://www.microsoft.com/en-us/research/wp-content/uploads/2009/09/ghc-parallel-tuning2.pdf)  
17. [GHC Memory Management](https://wiki.haskell.org/GHC/Memory_Management)  

---

## License

This project is licensed under the [MIT License](./LICENSE). You’re free to use, modify, and distribute this project; please see the `LICENSE` file for details. 

Enjoy exploring **Parallel MVC** in Haskell! If you have questions or issues, don’t hesitate to [open an issue](https://github.com/tony-feasts/parallel-mvc/issues). Happy coding!
