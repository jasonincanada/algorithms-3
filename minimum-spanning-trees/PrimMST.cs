// Prim's minimum spanning tree, naive O(mn) implementation

using System;
using System.Collections.Generic;
using System.Linq;

namespace jrh.Algorithms.PrimMinimumSpanningTree
{
    class PrimMST<T> where T : IEquatable<T>
    {
        private Graph<T> _graph;
        private ICollection<Graph<T>.WeightedEdge> _tree;

        public PrimMST(Graph<T> graph)
        {
            _graph = graph;
        }

        public void Search(Graph<T>.Vertex start)
        {
            _graph.SetUnexplored();
            start.SetExplored();

            // Accumulate the edges we've selected at each iteration of the main loop
            _tree = new List<Graph<T>.WeightedEdge>();

            var edges = _graph.EnumerableEdges();

            while (true)
            {
                // This is slightly more complex than Dijkstra's SPA because the edges are
                // undirected.  So we'll flip the source/target vertices when querying
                var frontier = edges
                    .Where(edge => edge.Source.Explored && !edge.Target.Explored
                                   || !edge.Source.Explored && edge.Target.Explored);

                var cheapest = frontier
                    .OrderBy(edge => edge.Weight)
                    .FirstOrDefault();

                if (cheapest == null)
                    break;

                _tree.Add(cheapest);

                if (!cheapest.Target.Explored)
                    cheapest.Target.SetExplored();
                else
                    cheapest.Source.SetExplored();
            }
        }

        public long TreeCost()
        {
            return _tree.Sum(edge => edge.Weight);
        }
    }
}