using OpenClose.Factories;
using OpenClose.Resources;
using OpenClose.Services;

Payroll.CalculateSalaryMonthly([
    EmployeeFactory.CreateEmployee("Pepito Pérez", 160,EmployeeType.FullTime),
    EmployeeFactory.CreateEmployee("Manuel Lopera", 180,EmployeeType.PartTime),
]);

Console.WriteLine("Press any key to finish...");
Console.ReadKey();