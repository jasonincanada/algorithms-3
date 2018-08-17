/* Algorithms 3 - Greedy Algorithms, Minimum Spanning Trees, and Dynamic Programming
   Stanford University via coursera.org

   Programming Assignment #3.3 - Find the maximum-weight independent set of a path graph 

   Author: Jason Hooper
*/

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace jrh.Algorithms.MaxWeightIndependentSet
{
    class VertexWithStuff
    {
        // The integer weight of the vertex
        public long Weight { get; private set; }

        // The maximum WIS of the graph up to and including this point (left-to-right traversal)
        public long CummulativeWeight { get; private set; }

        // Whether this vertex is included in the maximum weight independent set
        // for the path graph it belongs to
        public bool Included { get; private set; }

        public VertexWithStuff(int index, long weight)
        {
            Weight = weight;
            Included = false;
        }

        public override string ToString()
        {
            return $"{Weight} (now {CummulativeWeight}) - {Included}";
        }

        public void SetCummulativeWeight(long weight)
        {
            CummulativeWeight = weight;
        }

        public void SetIncluded()
        {
            Included = true;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            IEnumerable<long> weights = LongsFromFile("mwis.txt");
            IList<VertexWithStuff> graph = new List<VertexWithStuff>();

            int index = 0;            
            foreach (var weight in weights)
            {
                graph.Add(new VertexWithStuff(index, weight));
                index++;
            }

            // Base case
            graph[0].SetCummulativeWeight(graph[0].Weight);

            for (var i = 1; i < graph.Count; i++)
            {
                long oneAgo = graph[i - 1].CummulativeWeight;
                long twoAgo = i < 2 ? 0 : graph[i - 2].CummulativeWeight;
                long thisWeight = graph[i].Weight;

                long maxWeight = Math.Max(oneAgo, twoAgo + thisWeight);

                graph[i].SetCummulativeWeight(maxWeight);
            }

            // Second pass to identify the vertices in the WMIS
            int idx = graph.Count - 1;
            while (idx >= 0)
            {
                long oneAgo = idx < 1 ? 0 : graph[idx - 1].CummulativeWeight;
                long twoAgo = idx < 2 ? 0 : graph[idx - 2].CummulativeWeight;
                long thisWeight = graph[idx].Weight;

                if (oneAgo >= twoAgo + thisWeight)
                {
                    idx--;
                }
                else
                {
                    graph[idx].SetIncluded();
                    idx--;
                    idx--;
                }
            }

            // Test whether these (1-based index) vertices are included in the MWIS
            var testVertices = new int[] { 1, 2, 3, 4, 17, 117, 517, 997 };

            string bitstring = "";
            foreach (var test in testVertices)
            {
                if (graph[test - 1].Included)
                    bitstring += "1";
                else
                    bitstring += "0";
            }

            Console.WriteLine("{0}", bitstring);
        }

        static IEnumerable<long> LongsFromFile(string filename)
        {
            using (TextReader reader = File.OpenText(filename))
            {
                // Discard the first line which lists the number of vertices
                reader.ReadLine();

                string line;
                while ((line = reader.ReadLine()) != null)
                    yield return long.Parse(line);
            }
        }
    }
}
