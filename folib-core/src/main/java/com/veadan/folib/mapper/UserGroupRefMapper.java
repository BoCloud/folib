package com.veadan.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.veadan.folib.dto.RoleResourceRefDTO;
import com.veadan.folib.entity.UserGroupRef;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

 /**
 * 用户组关联表;(user_group_ref)表数据库访问层
 * @author veadan
 * @date : 2024-7-17
 */
@Mapper
public interface UserGroupRefMapper extends BaseMapper<UserGroupRef> {
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    UserGroupRef queryById(Long id);
    /** 
     * 分页查询指定行数据
     *
     * @param userGroupRef 查询条件
     * @param pageable 分页对象
     * @return 对象列表
     */
    IPage<UserGroupRef> queryAllByLimit(Page<UserGroupRef> pageable, @Param("entity") UserGroupRef userGroupRef);
    /** 
     * 统计总行数
     *
     * @param userGroupRef 查询条件
     * @return 总行数
     */
    long count(UserGroupRef userGroupRef);
    /** 
     * 新增数据
     *
     * @param userGroupRef 实例对象
     * @return 影响行数
     */
    int insert(UserGroupRef userGroupRef);
    /** 
     * 批量新增数据
     *
     * @param entities List<UserGroupRef> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<UserGroupRef> entities);
    /** 
     * 批量新增或按主键更新数据
     *
     * @param entities List<UserGroupRef> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<UserGroupRef> entities);
    /** 
     * 更新数据
     *
     * @param userGroupRef 实例对象
     * @return 影响行数
     */
    int update(UserGroupRef userGroupRef);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(Long id);

     List<RoleResourceRefDTO> queryPrivilegeByGroup(@Param("entityId")Long groupId,@Param("refType") String refType, @Param("roleIds") List<String> roleIds);

     List<UserGroupRef> queryByGroupIds(@Param("groupIds") List<Long> groupIds);
 }