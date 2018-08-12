using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace jrh.Algorithms.Clustering
{
    class Graph<T> where T : IEquatable<T>
    {
        public class Vertex
        {
            public T Obj { get; private set; }
            public Vertex Leader { get; private set; }

            public Vertex(T obj)
            {
                Obj = obj;

                // A new vertex is its own leader (it's in its own cluster)
                // until set otherwise
                Leader = this;
            }

            public void SetLeader(Vertex leader)
            {
                Leader = leader;
            }

            public override string ToString()
            {
                return $"{Obj.ToString()}";
            }
        }

        public int VertexCount()
        {
            return _vertices.Count;
        }

        public void ResetLeaders()
        {
            foreach (var vertex in _vertices)
                vertex.SetLeader(vertex);
        }

        public class WeightedEdge
        {
            public Vertex Source { get; private set; }
            public Vertex Target { get; private set; }
            public long Weight { get; private set; }

            public WeightedEdge(Vertex source, Vertex target, long weight)
            {
                Source = source;
                Target = target;
                Weight = weight;
            }

            public override string ToString()
            {
                return string.Format("{0} --{1}-- {2}",
                                     Source.ToString(),
                                     Weight,
                                     Target.ToString());
            }
        }

        public IEnumerable<Vertex> VerticesByLeader(Vertex leader)
        {
            return _vertices.Where(v => v.Leader == leader);
        }

        private ICollection<Vertex> _vertices;
        private ICollection<WeightedEdge> _edges;

        Graph(ICollection<Vertex> vertices)
        {
            _vertices = vertices;
            _edges = new List<WeightedEdge>();
        }

        void AddEdge(T from, T to, long weight)
        {
            Vertex source = GetVertex(from);
            Vertex target = GetVertex(to);
            WeightedEdge edge = new WeightedEdge(source, target, weight);

            _edges.Add(edge);
        }

        public Vertex GetVertex(T target)
        {
            var vertex = _vertices
                .Where(v => v.Obj.Equals(target))
                .FirstOrDefault();

            if (vertex == null)
                throw new ArgumentOutOfRangeException("Could not find vertex " + target.ToString());

            return vertex;
        }

        public IEnumerable<WeightedEdge> EnumerableEdges()
        {
            return _edges.AsEnumerable();
        }

        public static Graph<int> IntsFromFile(string filename)
        {
            ICollection<Tuple<int, int, long>> edges = new List<Tuple<int, int, long>>();

            int maxNodeNumber = 0;

            using (TextReader reader = File.OpenText(filename))
            {
                // Discard the first line which lists the number of edges
                reader.ReadLine();

                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    var nums = line.Split(' ');
                    int source = int.Parse(nums[0]);
                    int target = int.Parse(nums[1]);
                    long weight = long.Parse(nums[2]);

                    edges.Add(new Tuple<int, int, long>(source, target, weight));

                    if (source > maxNodeNumber)
                        maxNodeNumber = source;

                    if (target > maxNodeNumber)
                        maxNodeNumber = target;
                }
            }

            // Assume vertices are a contiguous list of integers up to the maximum
            // one we saw during the read
            var vertices = new List<Graph<int>.Vertex>();

            for (int i = 1; i <= maxNodeNumber; i++)
                vertices.Add(new Graph<int>.Vertex(i));

            var graph = new Graph<int>(vertices);

            foreach (var edge in edges)
                graph.AddEdge(edge.Item1, edge.Item2, edge.Item3);

            return graph;
        }
    }
}