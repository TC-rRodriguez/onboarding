namespace InterfaceSegregation.Interfaces;

/// <summary>
/// Interface representing a team member with the ability to review assignations.
/// </summary>
public interface ITeamMember
{
    /// <summary>
    /// Reviews the assignation of tasks or responsibilities.
    /// </summary>
    void ReviewAssignation();
}