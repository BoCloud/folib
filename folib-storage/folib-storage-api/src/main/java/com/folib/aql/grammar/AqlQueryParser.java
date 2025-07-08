package com.folib.aql.grammar;

import com.folib.data.criteria.QueryParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import com.folib.aql.grammar.AQLParser.QueryContext;
import com.folib.domain.ArtifactEntity;

/**
 * @author veadan
 *
 */
public class AqlQueryParser extends QueryParser<QueryContext, ArtifactEntity, AqlStatementVisitor>
{

    public AqlQueryParser(String query)
    {
        super(createParser(CharStreams.fromString(query)));
    }

    public static Parser createParser(CharStream is)
    {
        AQLLexer lexer = new AQLLexer(is);
        CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
        return new AQLParser(commonTokenStream);
    }

    @Override
    protected AqlStatementVisitor createTreeVisitor()
    {
        return new AqlStatementVisitor();
    }

    @Override
    protected QueryContext parseQueryTree(Parser parser)
    {
        return ((AQLParser) parser).query();
    }

}
