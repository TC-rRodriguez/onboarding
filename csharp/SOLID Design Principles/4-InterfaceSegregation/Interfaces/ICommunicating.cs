namespace InterfaceSegregation.Interfaces;

/// <summary>
/// Interface for communicating tasks.
/// </summary>
public interface ICommunicating : ITeamMember
{
    /// <summary>
    /// Method to handle communication.
    /// </summary>
    void Communicate();
}