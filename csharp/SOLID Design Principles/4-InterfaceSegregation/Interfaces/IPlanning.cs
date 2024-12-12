namespace InterfaceSegregation.Interfaces;

/// <summary>
/// Interface for planning tasks.
/// </summary>
public interface IPlanning : ITeamMember
{
    /// <summary>
    /// Method to handle planning.
    /// </summary>
    void Plan();
    
    /// <summary>
    /// Assigns tasks to team members.
    /// </summary>
    void AssignTasks();
}