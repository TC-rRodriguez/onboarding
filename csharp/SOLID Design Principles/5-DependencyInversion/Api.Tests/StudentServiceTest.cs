using System.Collections.Generic;
using System.Threading.Tasks;
using Moq;
using Xunit;
using Api.Interfaces;
using Api.Services;
using DependencyInversion;

namespace Api.Tests
{
    public class StudentServiceTest
    {
        [Fact]
        public async Task GetAllStudents_ReturnsAllStudents()
        {
            // Arrange
            var mockStudentRepository = new Mock<IStudentRepository>();
            var mockLogBook = new Mock<ILogBook>();
            var students = new List<Student>
            {
                new Student(1, "John Doe", new List<double> { 3.5, 4.0 }),
                new Student(2, "Jane Doe", new List<double> { 3.0, 3.5 })
            };
            mockStudentRepository.Setup(repo => repo.GetAll()).ReturnsAsync(students);
            var studentService = new StudentService(mockStudentRepository.Object, mockLogBook.Object);

            // Act
            var result = await studentService.GetAllStudents();

            // Assert
            Assert.Equal(students, result);
            mockLogBook.Verify(log => log.Add("Returning student's list"), Times.Once);
        }

        [Fact]
        public async Task AddStudent_AddsStudentAndLogsAction()
        {
            // Arrange
            var mockStudentRepository = new Mock<IStudentRepository>();
            var mockLogBook = new Mock<ILogBook>();
            var newStudent = new Student(3, "New Student", new List<double> { 3.5, 4.0 });
            var studentService = new StudentService(mockStudentRepository.Object, mockLogBook.Object);

            // Act
            await studentService.AddStudent(newStudent);

            // Assert
            mockStudentRepository.Verify(repo => repo.Add(newStudent), Times.Once);
            mockLogBook.Verify(log => log.Add($"The Student {newStudent.Fullname} have been added"), Times.Once);
        }
    }
}