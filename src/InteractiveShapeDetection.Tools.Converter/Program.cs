using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
namespace Converter
{
    class Program
    {
        static void Main(string[] args)
        {
            if(args.Length < 2)
            {
                Console.WriteLine("Input Missmatch!");
                Console.ReadKey();
                return;
            }

            string srcPath = args[0];
            string destPath = args[1];

            FileInfo inputInfo = new FileInfo(srcPath);


            if(inputInfo.Extension != ".ply")
            {
                Console.WriteLine("Unsupported FileType!");
                Console.ReadKey();
                return;
            }
            //string path = args[1];
            //Console.WriteLine(path);
            Console.WriteLine("Reading...");
            var ply = File.ReadAllLines(srcPath);

            Console.WriteLine("Converting...");
            var pts = ply.Skip(13).Select(line =>
            {
                var nums = line.Split(' ');

                var px = nums[0];
                var py = nums[1];
                var pz = nums[2];

                var r = nums[6];
                var g = nums[7];
                var b = nums[8];

                return px + " " + py + " " + pz + " 0 " + r +" " + g +" "+ b;
            }).ToArray();


            string[] numVertices = { "" + pts.Length };

            Console.WriteLine("Writing...");
            File.WriteAllLines(destPath, numVertices);
            File.AppendAllLines(destPath, pts);
            Console.Write("Finished!");
            Console.ReadKey();

           
        }
    }
}
