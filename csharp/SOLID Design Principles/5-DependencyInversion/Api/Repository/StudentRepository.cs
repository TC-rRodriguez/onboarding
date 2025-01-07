using System.Collections.ObjectModel;
using Api.Interfaces;

namespace DependencyInversion;

/// <summary>
/// Represents a repository for managing student data.
/// </summary>
public class StudentRepository : IStudentRepository
{
    private static ObservableCollection<Student> _collection;

    /// <summary>
    /// Initializes a new instance of the <see cref="StudentRepository"/> class.
    /// </summary>
    public StudentRepository()
    {
        InitData();
    }

    /// <summary>
    /// Initializes the student data collection.
    /// </summary>
    private static void InitData()
    {
        if (_collection != null) return;
            
        _collection = new ObservableCollection<Student>
        {
            new(1, "Pepito Pérez", new List<double> { 3, 4.5 }),
            new(2, "Mariana Lopera", new List<double> { 4, 5 }),
            new(3, "José Molina", new List<double> { 2, 3 })
        };
    }

    /// <summary>
    /// Gets all students.
    /// </summary>
    /// <returns>A task that represents the asynchronous operation. The task result contains a collection of students.</returns>
    public async Task<IEnumerable<Student>> GetAll()
    {
        return await Task.FromResult(_collection);
    }

    /// <summary>
    /// Adds a new student to the collection.
    /// </summary>
    /// <param name="student">The student to add.</param>
    /// <returns>A task that represents the asynchronous operation.</returns>
    public async Task Add(Student student)
    {
        _collection.Add(student);
        await Task.CompletedTask;
    }
}