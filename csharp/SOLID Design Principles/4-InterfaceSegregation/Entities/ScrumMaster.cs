using InterfaceSegregation.Interfaces;

namespace InterfaceSegregation.Entities;

public class ScrumMaster : TeamMember, IPlanning, ICommunicating
{
    /// <summary>
    /// Plans user stories.
    /// </summary>
    public void Plan()
    {
        Console.WriteLine("I'm planning user stories");
    }

    /// <summary>
    /// Assigns tasks to team members.
    /// </summary>
    public void AssignTasks()
    {
        Console.WriteLine("I'm assigning tasks to team members");
    }

    /// <summary>
    /// Communicates with the team.
    /// </summary>
    public void Communicate()
    {
        Console.WriteLine("I'm talking to the team");
    }
}