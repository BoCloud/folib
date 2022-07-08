package com.veadan.folib.nuget.filter;

import com.veadan.folib.data.criteria.QueryParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import com.veadan.folib.domain.ArtifactEntity;

/**
 * @author xuxinping
 *
 */
public class NugetODataFilterQueryParser extends QueryParser<NugetODataFilterParser.FilterContext, ArtifactEntity, NugetODataFilterVisitorImpl>
{

    public NugetODataFilterQueryParser(String query)
    {
        super(createParser(CharStreams.fromString(query)));
    }

    public static Parser createParser(CharStream is)
    {
        NugetODataFilterLexer lexer = new NugetODataFilterLexer(is);
        CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
        return new NugetODataFilterParser(commonTokenStream);
    }

    @Override
    protected NugetODataFilterVisitorImpl createTreeVisitor()
    {
        return new NugetODataFilterVisitorImpl();
    }

    @Override
    protected NugetODataFilterParser.FilterContext parseQueryTree(Parser parser)
    {
        return ((NugetODataFilterParser) parser).filter();
    }
}
