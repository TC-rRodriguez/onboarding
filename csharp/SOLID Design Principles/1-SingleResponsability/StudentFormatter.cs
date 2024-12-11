using System.Text;

namespace SingleResponsability;

public static class StudentFormatter
{
    /// <summary>
    /// Formats a collection of students into a CSV string.
    /// </summary>
    /// <param name="students">The collection of students to format.</param>
    /// <returns>A CSV string representation of the students.</returns>
    public static string Format(IEnumerable<Student> students)
    {
        var sb = new StringBuilder();
        sb.AppendLine("Id;Fullname;Grades");

        foreach (var student in students)
        {
            sb.AppendLine(student.ToString());
        }
        
        return sb.ToString();
    }
}