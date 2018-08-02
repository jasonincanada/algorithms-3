/* Algorithms 3 - Greedy Algorithms, Minimum Spanning Trees, and Dynamic Programming
   Stanford University via coursera.org

   Programming Assignment #1.1, 1.2 - Minimizing weighted sum of job completion times

   Author: Jason Hooper
*/

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace jrh.Algorithms.JobScheduling
{
    class Job
    {
        public int Weight { get; private set; }
        public int Length { get; private set; }

        public Job(int weight, int length)
        {
            // The ratio comparer divides by length, so ensure this is non-zero
            if (length == 0)
                throw new ArgumentOutOfRangeException("Length must be greater than 0");

            Weight = weight;
            Length = length;
        }
    }

    // A custom comparer that compares jobs by the difference of their weight
    // and length, tie-breaking on the weight if the differences are equal
    class DifferenceComparer : IComparer<Job>
    {
        public int Compare(Job x, Job y)
        {
            var xDifference = x.Weight - x.Length;
            var yDifference = y.Weight - y.Length;

            if (yDifference == xDifference)
                return x.Weight.CompareTo(y.Weight);

            return xDifference.CompareTo(yDifference);
        }
    }

    // A custom comparer that compares jobs by the ratio of their weight
    // to length, no tie-breaking necessary (per problem description)
    class RatioComparer : IComparer<Job>
    {
        public int Compare(Job x, Job y)
        {
            double xRatio = (double)x.Weight / x.Length;
            double yRatio = (double)y.Weight / y.Length;

            return xRatio.CompareTo(yRatio);
        }
    }

    // A wrapper around a list of jobs that provides a method to calculate the
    // sum of weighted completion times using a custom comparer
    class Schedule
    {
        private IEnumerable<Job> _jobs;

        public Schedule(IEnumerable<Job> jobs)
        {
            _jobs = jobs;
        }

        public long WeightedCompletionTimeSum(IComparer<Job> comparer)
        {
            long runningCompletionTime = 0;
            long runningWeightedSum = 0;

            var sorted = _jobs.OrderByDescending(x => x, comparer);

            foreach (var job in sorted)
            {
                runningCompletionTime += job.Length;
                runningWeightedSum += job.Weight * runningCompletionTime;
            }

            return runningWeightedSum;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            IEnumerable<Job> jobs = JobsFromFile("jobs.txt");

            Schedule schedule = new Schedule(jobs);

            long differenceSum = schedule.WeightedCompletionTimeSum(new DifferenceComparer());
            long ratioSum = schedule.WeightedCompletionTimeSum(new RatioComparer());

            Console.WriteLine("Sum of weighted completion times:");
            Console.WriteLine($"\tDifference ordering: {differenceSum}");
            Console.WriteLine($"\t     Ratio ordering: {ratioSum}");
        }

        static IEnumerable<Job> JobsFromFile(string filename)
        {
            using (TextReader reader = File.OpenText(filename))
            {
                // Discard the first line which is the number of jobs in the file, we'll
                // just keep returning jobs until the end of file
                reader.ReadLine();

                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    string[] nums = line.Split(' ');

                    int weight = int.Parse(nums[0]);
                    int length = int.Parse(nums[1]);

                    yield return new Job(weight, length);
                }
            }
        }
    }
}