using Api.Interfaces;
using DependencyInversion;

namespace Api.Services;

/// <summary>
/// Provides services for managing students.
/// </summary>
public class StudentService : IStudentService
{
    private readonly IStudentRepository _studentRepository;
    private readonly ILogBook _logBook;
    
    /// <summary>
    /// Initializes a new instance of the <see cref="StudentService"/> class.
    /// </summary>
    /// <param name="studentRepository">The student repository.</param>
    /// <param name="logBook">The logbook for recording actions.</param>
    public StudentService(IStudentRepository studentRepository, ILogBook logBook)
    {
        _studentRepository = studentRepository;
        _logBook = logBook;
    }
    
    /// <summary>
    /// Gets all students.
    /// </summary>
    /// <returns>A task that represents the asynchronous operation. The task result contains a collection of students.</returns>
    public async Task<IEnumerable<Student>> GetAllStudents()
    {
        _logBook.Add("Returning student's list");
        return await _studentRepository.GetAll();
    }

    /// <summary>
    /// Adds a new student.
    /// </summary>
    /// <param name="student">The student to add.</param>
    /// <returns>A task that represents the asynchronous operation.</returns>
    public async Task AddStudent(Student student)
    {
        await _studentRepository.Add(student);
        _logBook.Add($"The Student {student.Fullname} have been added");
    }
}