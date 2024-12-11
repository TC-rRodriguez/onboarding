namespace SingleResponsability;

/// <summary>
/// Provides functionality to export student data to a CSV file.
/// </summary>
public class StudentExporter
{
    private readonly StudentRepository _studentRepository;
    
    /// <summary>
    /// Initializes a new instance of the <see cref="StudentExporter"/> class with the specified student repository.
    /// </summary>
    /// <param name="studentRepository">The student repository to use for retrieving student data.</param>
    public StudentExporter(StudentRepository studentRepository)
    {
        _studentRepository = studentRepository;
    }

    /// <summary>
    /// Exports the student data to a CSV file.
    /// </summary>
    public void Export()
    {
        var students = _studentRepository.GetAll();
        var data = StudentFormatter.Format(students);
        SaveToCsv.Save(data, "students");
    }
}