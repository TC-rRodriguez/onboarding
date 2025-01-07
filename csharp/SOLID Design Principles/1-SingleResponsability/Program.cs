using SingleResponsability;

StudentRepository studentRepository = new();
StudentExporter studentExporter = new(studentRepository);


studentExporter.Export();

Console.WriteLine("Process completed!");

Console.WriteLine("Press any key to finish...");
Console.ReadKey();