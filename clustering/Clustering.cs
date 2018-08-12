using System;
using System.Linq;

namespace jrh.Algorithms.Clustering
{
    class Clustering<T> where T : IEquatable<T>
    {
        private Graph<T> _graph;

        public Clustering(Graph<T> graph)
        {
            _graph = graph;
        }

        public long SpacingForKClusters(int target)
        {            
            var edges = _graph.EnumerableEdges();
            var sorted = edges.OrderBy(x => x.Weight).ToList();

            _graph.ResetLeaders();

            // Each vertex starts off in its own cluster, so the number of
            // clusters we start with is just the number of vertices
            var clusters = _graph.VertexCount();

            for (var i = 0; i < sorted.Count; i++)
            {
                var edge = sorted[i];

                if (!IsSeparated(edge.Source, edge.Target))
                    continue;

                // Union these two clusters together, naively
                MergeClusters(edge.Source, edge.Target);
                clusters--;

                if (clusters <= target)
                {
                    // We have our target number of clusters, now find the first
                    // edge that spans distinct clusters, this is our spacing
                    for (var j = i; j < sorted.Count; j++)
                    {
                        var e = sorted[j];

                        if (IsSeparated(e.Source, e.Target))
                            return e.Weight;
                    }

                    // TODO: What does it mean that the for loop didn't find the spacing?
                    throw new NotImplementedException("Met target clustering count but didn't find a separated edge");
                }
            }

            // TODO: What does it mean for code execution to get here?
            throw new NotImplementedException("Shouldn't get here");
        }

        // Naively merge clusters by reassigning leaders
        public void MergeClusters(Graph<T>.Vertex source, Graph<T>.Vertex target)
        {
            var sourceLeader = source.Leader;
            var targetLeader = target.Leader;

            var cluster = _graph.VerticesByLeader(source.Leader);

            foreach (var vertex in cluster)
                vertex.SetLeader(targetLeader);
        }

        // Two vertices are separated iff they belong to distinct clusters
        private bool IsSeparated(Graph<T>.Vertex source, Graph<T>.Vertex target)
        {
            return source.Leader != target.Leader;
        }
    }
}
