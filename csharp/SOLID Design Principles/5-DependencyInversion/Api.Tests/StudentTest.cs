using System.Collections.Generic;
using Xunit;
using DependencyInversion;

namespace Api.Tests
{
    public class StudentTest
    {
        [Fact]
        public void StudentInitialization_DefaultConstructor()
        {
            // Arrange & Act
            var student = new Student();

            // Assert
            Assert.Equal(0, student.Id);
            Assert.Equal(string.Empty, student.Fullname);
            Assert.NotNull(student.Grades);
            Assert.Empty(student.Grades);
        }

        [Fact]
        public void StudentInitialization_ParameterizedConstructor()
        {
            // Arrange
            var id = 1;
            var fullname = "John Doe";
            var grades = new List<double> { 3.5, 4.0, 4.5 };

            // Act
            var student = new Student(id, fullname, grades);

            // Assert
            Assert.Equal(id, student.Id);
            Assert.Equal(fullname, student.Fullname);
            Assert.Equal(grades, student.Grades);
        }

        [Fact]
        public void Student_SetProperties()
        {
            // Arrange
            var student = new Student();
            var id = 2;
            var fullname = "Jane Doe";
            var grades = new List<double> { 2.5, 3.0, 3.5 };

            // Act
            student.Id = id;
            student.Fullname = fullname;
            student.Grades = grades;

            // Assert
            Assert.Equal(id, student.Id);
            Assert.Equal(fullname, student.Fullname);
            Assert.Equal(grades, student.Grades);
        }
    }
}