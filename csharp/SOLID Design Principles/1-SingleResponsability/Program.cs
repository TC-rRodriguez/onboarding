using SingleResponsability;

StudentRepository studentRepository = new();
studentRepository.Export();
Console.WriteLine("Process completed!");

Console.WriteLine("Press any key to finish...");
Console.ReadKey();
