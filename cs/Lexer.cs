using System.Text;

namespace Morth;

public static class Lexer
{
    private static IEnumerable<Token> LexLine(string filePath, int lineIndex, string line)
    {
        for (int columnIndex = 0; columnIndex < line.Length;)
        {
            var tokenStart = line[columnIndex..]
                .TakeWhile(c => char.IsWhiteSpace(c))
                .Count();
            columnIndex += tokenStart;

            var tokenLength = line[columnIndex..].TakeWhile(c => !char.IsWhiteSpace(c)).Count();
            if (tokenLength == 0)
            {
                yield break;
            }

            yield return new Token(
                line[columnIndex..(columnIndex + tokenLength)],
                filePath,
                lineIndex + 1,
                columnIndex + 1
            );
            columnIndex += tokenLength;
        }
    }

    public static IEnumerable<Token> LexFile(string path)
    {
        var lines = File.ReadAllLines(path);

        for (int i = 0; i < lines.Length; i++)
        {
            string line = lines[i];
            foreach (var token in LexLine(path, i, line.Split("//")[0]))
            {
                yield return token;
            }
        }

        yield break;
    }
}
