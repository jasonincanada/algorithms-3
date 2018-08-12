/* Algorithms 3 - Greedy Algorithms, Minimum Spanning Trees, and Dynamic Programming
   Stanford University via coursera.org

   Programming Assignment #2.1 - K-Clustering

   Remarks:  

   Author: Jason Hooper
*/

using System;

namespace jrh.Algorithms.Clustering
{
    class Program
    {
        static void Main(string[] args)
        {
            var graph = Graph<int>.IntsFromFile("clustering1.txt");
            var clustering = new Clustering<int>(graph);

            Console.WriteLine("Spacing for k = 2: {0}", clustering.SpacingForKClusters(target: 2));
            Console.WriteLine("Spacing for k = 3: {0}", clustering.SpacingForKClusters(target: 3));
            Console.WriteLine("Spacing for k = 4: {0}", clustering.SpacingForKClusters(target: 4));
            Console.WriteLine("Spacing for k = 5: {0}", clustering.SpacingForKClusters(target: 5));
        }
    }
}
