package com.folib.aql.grammar;

import com.folib.domain.ArtifactEntryExpressionBuilder;
import org.antlr.v4.runtime.tree.TerminalNode;
import com.folib.aql.grammar.AQLParser.TokenExpContext;
import com.folib.aql.grammar.AQLParser.TokenKeyContext;
import com.folib.aql.grammar.AQLParser.TokenValueContext;

/**
 * @author veadan
 *
 */
public class AqlExpressionVisitor extends AQLBaseVisitor<ArtifactEntryExpressionBuilder>
{

    private ArtifactEntryExpressionBuilder expressionBuilder = new ArtifactEntryExpressionBuilder(
            new AqlExpressionDialect());

    @Override
    public ArtifactEntryExpressionBuilder visitTokenExp(TokenExpContext ctx)
    {
        
        
        TokenKeyContext tokenKey;
        TerminalNode layout;
        if ((tokenKey = ctx.tokenKey()) != null)
        {
            visitTokenKey(tokenKey);
            visitTokenValue(ctx.tokenValue());
        }
        else if ((layout = ctx.LAYOUT()) != null)
        {
            expressionBuilder = expressionBuilder.of(layout.getText()).with(ctx.layoutValue().getText());
        }
        
        return expressionBuilder;
    }
    
    

    @Override
    public ArtifactEntryExpressionBuilder visitTokenValue(TokenValueContext ctx)
    {
        return expressionBuilder = expressionBuilder.with(ctx.getText());
    }

    @Override
    public ArtifactEntryExpressionBuilder visitTokenKey(TokenKeyContext ctx)
    {
        String attribute = null;
        if (ctx.layoutCoordinateKeyword() != null)
        {
            attribute = ctx.layoutCoordinateKeyword().getText();
        }
        else if (ctx.tokenKeyword() != null)
        {
            attribute = ctx.tokenKeyword().getText();
        }
        return expressionBuilder = expressionBuilder.of(attribute);
    }

    public ArtifactEntryExpressionBuilder getExpressionBuilder()
    {
        return expressionBuilder;
    }

}
