using InterfaceSegregation.Interfaces;

namespace InterfaceSegregation.Entities;

/// <summary>
/// Represents a team member who can review the assignation of tasks.
/// </summary>
public abstract class TeamMember : ITeamMember
{
    /// <summary>
    /// Reviews the assignation of tasks.
    /// </summary>
    public virtual void ReviewAssignation()
    {
        Console.WriteLine("I'm reviewing the assignation of tasks");
    }
}