using DependencyInversion;

namespace Api.Interfaces;

/// <summary>
/// Represents a repository for managing student data.
/// </summary>
public interface IStudentRepository
{
    /// <summary>
    /// Gets all students.
    /// </summary>
    /// <returns>A task that represents the asynchronous operation. The task result contains a collection of students.</returns>
    Task<IEnumerable<Student>> GetAll();

    /// <summary>
    /// Adds a new student.
    /// </summary>
    /// <param name="student">The student to add.</param>
    /// <returns>A task that represents the asynchronous operation.</returns>
    Task Add(Student student);
}