using InterfaceSegregation.Interfaces;

namespace InterfaceSegregation.Entities;

/// <summary>
/// Represents a tester who tests the functionalities developed.
/// </summary>
public class Tester : TeamMember, ITesting
{
    /// <summary>
    /// Tests the functionalities developed.
    /// </summary>
    public void Test()
    {
        Console.WriteLine("I'm testing the functionalities developed");
    }
}