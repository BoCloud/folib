package com.folib.domain;

import com.folib.data.criteria.ExpressionBuilder;
import com.folib.data.criteria.ExpressionDialect;

/**
 * @author veadan
 *
 */
public class ArtifactEntryExpressionBuilder extends ExpressionBuilder<ArtifactEntryExpressionBuilder, ArtifactEntity>
{

    public ArtifactEntryExpressionBuilder(ExpressionDialect dialect)
    {
        super(ArtifactEntity.class, dialect);
    }

    public ArtifactEntryExpressionBuilder()
    {
        super(ArtifactEntity.class);
    }

    @Override
    public ArtifactEntryExpressionBuilder of(String attribute)
    {
        return super.of(attribute);
    }
    
}
