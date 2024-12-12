using InterfaceSegregation.Interfaces;

namespace InterfaceSegregation.Entities;

/// <summary>
/// Represents a developer who is responsible for developing functionalities.
/// </summary>
public class Developer : TeamMember, IDeveloping
{
    /// <summary>
    /// Develops the required functionalities.
    /// </summary>
    public void Develop()
    {
        Console.WriteLine("I'm developing the functionalities required");
    }
}