namespace Api.Interfaces;

/// <summary>
/// Represents a logbook for recording descriptions.
/// </summary>
public interface ILogBook
{
    /// <summary>
    /// Adds a description to the logbook.
    /// </summary>
    /// <param name="description">The description to add.</param>
    void Add(string description);
}