package com.veadan.folib.domain.block;

import com.veadan.folib.controllers.block.req.AllowlistDenylistBlockQueryReq;
import com.veadan.folib.controllers.block.req.AllowlistDenylistBlockReq;
import com.veadan.folib.controllers.block.res.AllowlistDenylistBlockRes;
import com.veadan.folib.entity.AllowlistDenylistBlock;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;


/**
 * 黑白名单阻断;(allowlist_denylist_block)表服务接口
 * @author : http://www.chiner.pro
 * @date : 2024-12-6
 */
public interface AllowlistDenylistBlockService{
    /**
     * 查询单条数据
     *
     * @param allowlistDenylistBlockReq 主键
     * @return 实例对象
     */
    AllowlistDenylistBlockRes queryAllowlistDenylistBlock(AllowlistDenylistBlockReq allowlistDenylistBlockReq);
    /**
     * 分页查询
     *
     * @param allowlistDenylistBlock 筛选条件
     * @return 查询结果
     */
    Page<AllowlistDenylistBlockRes> paginQuery(AllowlistDenylistBlockQueryReq allowlistDenylistBlock);
    /**
     * 新增数据
     *
     * @param allowlistDenylistBlock 实例对象
     * @return 实例对象
     */
    AllowlistDenylistBlockRes insert(AllowlistDenylistBlockReq allowlistDenylistBlock);
    /**
     * 更新数据
     *
     * @param allowlistDenylistBlock 实例对象
     * @return 实例对象
     */
    AllowlistDenylistBlockRes update(AllowlistDenylistBlockReq allowlistDenylistBlock);
    /**
     * 删除数据
     *
     * @param req
     * @return 是否成功
     */
    boolean deleteAllowlistDenylistBlock(AllowlistDenylistBlockReq req);
}
