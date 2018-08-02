/* Algorithms 3 - Greedy Algorithms, Minimum Spanning Trees, and Dynamic Programming
   Stanford University via coursera.org

   Programming Assignment #1.3 - Prim's mimimum spanning tree

   Remarks:  This uses the naive O(mn) frontier search algorithm which is sufficient
             for this data set.  An improved version would use a min-heap to track
             unseen edges.

   Author: Jason Hooper
*/

using System;

namespace jrh.Algorithms.PrimMinimumSpanningTree
{
    class Program
    {
        static void Main(string[] args)
        {
            var graph = Graph<int>.IntsFromFile("edges.txt");
            var prim = new PrimMST<int>(graph);

            // The starting vertex is arbitrary so we'll pick the first
            var start = graph.GetVertex(1);

            prim.Search(start);

            Console.WriteLine("Tree cost: {0}", prim.TreeCost());
        }
    }
}