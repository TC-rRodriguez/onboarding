using System.ComponentModel.DataAnnotations;

namespace DependencyInversion;

/// <summary>
/// Represents a student with an ID, full name, and grades.
/// </summary>
public class Student
{
    /// <summary>
    /// Gets or sets the student ID.
    /// </summary>
    public int Id { get; set; }

    /// <summary>
    /// Gets or sets the full name of the student.
    /// </summary>
    [Required]
    [StringLength(100)]
    public string Fullname { get; set; }

    /// <summary>
    /// Gets or sets the list of grades for the student.
    /// </summary>
    [Required]
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
}
