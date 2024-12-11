namespace SingleResponsability;

/// <summary>
/// Repository class for managing Student entities.
/// </summary>
public class StudentRepository
{
    private static FakeStorage<Student> storage;

    /// <summary>
    /// Initializes a new instance of the <see cref="StudentRepository"/> class.
    /// </summary>
    public StudentRepository()
    {
        storage = new FakeStorage<Student>();
        InitData();
    }

    /// <summary>
    /// Initializes the repository with sample data.
    /// </summary>
    private static void InitData()
    {
        storage.Add(new Student(1, "Pepito Pérez", [3, 4.5]));
        storage.Add(new Student(2, "Mariana Lopera", [4, 5]));
        storage.Add(new Student(3, "José Molina", [2, 3]));
    }

    /// <summary>
    /// Adds a new student to the repository.
    /// </summary>
    /// <param name="student">The student to add.</param>
    /// <returns>The added student.</returns>
    public Student Add(Student student)
    {
        return storage.Add(student);
    }
    
    /// <summary>
    /// Removes a student from the repository.
    /// </summary>
    /// <param name="student">The student to remove.</param>
    /// <returns>The removed student.</returns>
    public Student Remove(Student student)
    {
        return storage.Remove(student);
    }

    /// <summary>
    /// Gets all students from the repository.
    /// </summary>
    /// <returns>An enumerable collection of students.</returns>
    public IEnumerable<Student> GetAll() 
    {
        return storage.GetAll();
    }
}