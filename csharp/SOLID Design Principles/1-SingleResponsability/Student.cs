namespace SingleResponsability;

/// <summary>
/// Represents a student with an ID, full name, and a list of grades.
/// </summary>
/// <property name="Id">Gets or sets the student ID.</property>
/// <property name="Fullname">Gets or sets the full name of the student.</property>
/// <property name="Grades">Gets or sets the list of grades for the student.</property>
public class Student
{
    public int Id { get; set; }
    public string Fullname { get; set; }
    public List<double> Grades { get; set; }

    /// <summary>
    /// Initializes a new instance of the <see cref="Student"/> class.
    /// </summary>
    public Student()
    {
        Fullname = string.Empty;
        Grades = [];
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="Student"/> class with specified ID, full name, and grades.
    /// </summary>
    /// <param name="id">The student ID.</param>
    /// <param name="fullname">The full name of the student.</param>
    /// <param name="grades">The list of grades for the student.</param>
    public Student(int id, string fullname, List<double> grades)
    {
        Id = id;
        Fullname = fullname;
        Grades = grades;
    }

    /// <summary>
    /// Returns a string representation of the student.
    /// </summary>
    /// <returns>A string containing the student ID, full name, and grades separated by semicolons.</returns>
    public override string ToString()
    {
        return $"{Id};{Fullname};{string.Join("|", Grades)}";
    }
}