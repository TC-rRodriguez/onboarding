using Api.Interfaces;
using Microsoft.AspNetCore.Mvc;

namespace DependencyInversion.Controllers;

[ApiController, Route("student")]
public class StudentController : ControllerBase
{
    private readonly IStudentService _studentService;
    
    /// <summary>
    /// Initializes a new instance of the <see cref="StudentController"/> class.
    /// </summary>
    /// <param name="studentService">The student service.</param>
    public StudentController(IStudentService studentService)
    {
        _studentService = studentService;
    }

    /// <summary>
    /// Gets all students.
    /// </summary>
    /// <returns>A collection of students.</returns>
    [HttpGet]
    public async Task<IEnumerable<Student>> Get()
    {
        return await _studentService.GetAllStudents();
    }

    /// <summary>
    /// Adds a new student.
    /// </summary>
    /// <param name="student">The student to add.</param>
    [HttpPost]
    public async Task Add([FromBody]Student student)
    {
        await _studentService.AddStudent(student);
    }
}
