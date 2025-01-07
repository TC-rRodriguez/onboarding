using System.Text;
using Api.Interfaces;

namespace DependencyInversion;

/// <summary>
/// Represents a logbook for recording descriptions.
/// </summary>
public class Logbook : ILogBook
{
    /// <summary>
    /// Adds a description to the logbook.
    /// </summary>
    /// <param name="description">The description to add.</param>
    public void Add(string description)
    {
        var filePath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "logbook.txt");
        File.AppendAllText(filePath, $"{description}\n", Encoding.Unicode);
    }
}