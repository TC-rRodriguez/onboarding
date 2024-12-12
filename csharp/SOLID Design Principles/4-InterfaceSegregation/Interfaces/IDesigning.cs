namespace InterfaceSegregation.Interfaces;

/// <summary>
/// Interface for designing tasks.
/// </summary>
public interface IDesigning : ITeamMember
{
    /// <summary>
    /// Method to handle design tasks.
    /// </summary>
    void Design();
}