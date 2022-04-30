package com.veadan.folib.domain;

import com.veadan.folib.data.criteria.ExpressionBuilder;
import com.veadan.folib.data.criteria.ExpressionDialect;

/**
 * @author sbespalov
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
