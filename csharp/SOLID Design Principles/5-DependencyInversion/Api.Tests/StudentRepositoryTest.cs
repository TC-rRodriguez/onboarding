using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Xunit;
using DependencyInversion;

namespace Api.Tests
{
    public class StudentRepositoryTest
    {
        [Fact]
        public async Task GetAll_ReturnsAllStudents()
        {
            // Arrange
            var repository = new StudentRepository();

            // Act
            var students = await repository.GetAll();

            // Assert
            Assert.NotNull(students);
            Assert.Equal(3, students.Count());
        }

        [Fact]
        public async Task Add_AddsStudentToCollection()
        {
            // Arrange
            var repository = new StudentRepository();
            var newStudent = new Student(4, "New Student", new List<double> { 3.5, 4.0 });

            // Act
            await repository.Add(newStudent);
            var students = await repository.GetAll();

            // Assert
            Assert.Contains(newStudent, students);
        }
    }
}