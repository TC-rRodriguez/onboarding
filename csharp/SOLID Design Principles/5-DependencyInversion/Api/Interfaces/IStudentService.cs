using DependencyInversion;

namespace Api.Interfaces;

/// <summary>
/// Represents a service for managing student data.
/// </summary>
public interface IStudentService
{
    /// <summary>
    /// Gets all students.
    /// </summary>
    /// <returns>A task that represents the asynchronous operation. The task result contains a collection of students.</returns>
    Task<IEnumerable<Student>> GetAllStudents();

    /// <summary>
    /// Adds a new student.
    /// </summary>
    /// <param name="student">The student to add.</param>
    /// <returns>A task that represents the asynchronous operation.</returns>
    Task AddStudent(Student student);
}