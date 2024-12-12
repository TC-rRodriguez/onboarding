namespace InterfaceSegregation.Interfaces;

/// <summary>
/// Interface for developing tasks.
/// </summary>
public interface IDeveloping : ITeamMember
{
    /// <summary>
    /// Method to handle development.
    /// </summary>
    void Develop();
}