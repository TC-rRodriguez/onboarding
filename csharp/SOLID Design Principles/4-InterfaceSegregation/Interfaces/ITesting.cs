namespace InterfaceSegregation.Interfaces;

/// <summary>
/// Represents an interface for testing functionality.
/// </summary>
public interface ITesting : ITeamMember
{
    /// <summary>
    /// Executes the test.
    /// </summary>
    void Test();
}