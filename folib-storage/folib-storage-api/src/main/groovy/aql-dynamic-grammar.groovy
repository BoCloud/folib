import com.folib.artifact.coordinates.ArtifactLayoutLocator
import org.jtwig.JtwigTemplate
import org.jtwig.JtwigModel


def layoutMap = ArtifactLayoutLocator.getLayoutEntityMap()

layoutMap.each{ k, v -> println "${v.getArtifactCoordinates()}" }

JtwigTemplate template = JtwigTemplate.fileTemplate("$project.basedir/src/main/twig/AQL.g4.twig")
JtwigModel model = JtwigModel.newModel().with("layoutMap", layoutMap)

new File("$project.basedir/target/antlr4/com/folib/aql/grammar").mkdirs()

def out = new File("$project.basedir/target/antlr4/com/folib/aql/grammar","AQL.g4").newOutputStream()
try
{
    template.render(model, out)
}
finally
{
    out.close()
}
