package com.veadan.folib.common.handler;

import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedJdbcTypes;
import org.apache.ibatis.type.MappedTypes;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

/**
 * @Author: fengmg
 * @Date: 2024/8/1 17:25
 * @Description:
 */
@MappedTypes(List.class)
@MappedJdbcTypes(JdbcType.VARCHAR)
public class StringListTypeHandler extends BaseTypeHandler<List<String>> {
    @Override
    public void setNonNullParameter(PreparedStatement preparedStatement, int i, List<String> strings, JdbcType jdbcType) throws SQLException {
        preparedStatement.setString(i, String.join(",", strings));
    }

    @Override
    public List<String> getNullableResult(ResultSet resultSet, String s) throws SQLException {
        String value = resultSet.getString(s);
        return value != null ? Arrays.asList(value.split(",")) : null;

    }

    @Override
    public List<String> getNullableResult(ResultSet resultSet, int i) throws SQLException {
        String value = resultSet.getString(i);
        return value != null ? Arrays.asList(value.split(",")) : null;
    }

    @Override
    public List<String> getNullableResult(CallableStatement callableStatement, int i) throws SQLException {
        String value = callableStatement.getString(i);
        return value != null ? Arrays.asList(value.split(",")) : null;

    }


}
