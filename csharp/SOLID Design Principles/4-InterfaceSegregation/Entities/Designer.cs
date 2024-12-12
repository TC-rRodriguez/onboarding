using InterfaceSegregation.Interfaces;

namespace InterfaceSegregation.Entities;

/// <summary>
/// Represents a designer who is responsible for designing functionalities.
/// </summary>
public class Designer : TeamMember, IDesigning
{
    /// <summary>
    /// Designs the required functionalities.
    /// </summary>
    public void Design()
    {
        Console.WriteLine("I'm designing the required functionalities");
    }
}