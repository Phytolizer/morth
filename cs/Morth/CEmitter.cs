namespace Morth;

public class CEmitter
{
    private int _indentLevel = 0;
    private readonly TextWriter _fp;

    public CEmitter(TextWriter fp)
    {
        _fp = fp;
    }

    public void AddIndent()
    {
        _indentLevel += 4;
    }

    public void RemoveIndent()
    {
        _indentLevel -= 4;
    }

    public void Emit(string text)
    {
        for (int i = 0; i < _indentLevel; i++)
        {
            _fp.Write(' ');
        }
        _fp.WriteLine(text);
    }

    public void EmitNoIndent(string text)
    {
        _fp.WriteLine(text);
    }
}
