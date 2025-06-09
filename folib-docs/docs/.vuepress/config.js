const contentExtractor = require('./plugins/content-extractor');
module.exports = {
  title: 'Fo Library',
  description: 'Fo Library',
  dest: '../folib-web-core/src/main/resources/docs',
  base: '/help/',
  theme: './theme',
  head: [
    ['link', { rel: 'icon', href: '/favicon.ico' }]
  ],
  themeConfig: {
    logo: '/logo.png',
    docsDir: 'docs',
    displayAllHeaders: true,
    editLinks: false,
    editLinkText: '',
    searchPlaceholder:'请输入关键字符',
    nav: [ // 顶部菜单
      { text: '产品指南', link: '/docs/base/artifact-introduction', type:'productGuide' },
      { text: 'Open API', link: '/api/user/create', type: 'openApi' },
      { text: '常见问题', link: '/qa/best-practice', type: 'QA' },
    ],
    lastUpdated: 'folib-2.0',
    sidebar:
    {
      // 产品指南
      productGuide: [ // 左侧菜单
        // 基础知识
        {
          title: '基础知识',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '什么是制品?',
              link: '/docs/base/artifact-introduction'
            }, {
              title: '什么是制品库?',
              link: '/docs/base/library-introduction'
            }, {
              title: '常用名词定义',
              link: '/docs/base/warehouse-type-introduction'
            },
          ]
        },
        // 存储空间管理
        {
          title: '存储空间管理',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '空间概述',
              link: '/docs/storage-space/storage-space-outline'
            }, {
              title: '操作指南',
              link: '/docs/storage-space/storage-space-operation'
            },
          ]
        },
        // 制品仓库管理
        {
          title: '制品仓库管理',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '仓库概述',
              link: '/docs/warehouse/warehouse-outline'
            }, {
              title: '操作指南',
              link: '/docs/warehouse/warehouse-operation'
            }
          ]
        },
        // 制品文件管理
        {
          title: '制品文件管理',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '制品概述',
              link: '/docs/warefile/warefile-outline'
            }, {
              title: '操作指南',
              link: '/docs/warefile/warefile-operation'
            }
          ]
        },
        // 搜索操作指南
        {
          title: '搜索操作方法',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '搜索概述',
              link: '/docs/search-summary/search-summary-outline'
            }, {
              title: '仓库搜索',
              link: '/docs/search-summary/search-summary-warehouse'
            }, {
              title: '制品搜索',
              link: '/docs/search-summary/search-summary-warefile'
            }
          ]
        },
        // 安全扫描
        {
          title: '安全扫描',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '扫描概述',
              link: '/docs/scanning/scan_overview'
            }, {
              title: '仓库扫描详情',
              link: '/docs/scanning/repository_scan_status'
            }
          ]
        },
        // 制品分析
        {
          title: '制品分析',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '制品分析概述',
              link: '/docs/artifact-analysis/artifact-analysis-overview'
            },
            {
              title: '制品扫描',
              link: '/docs/artifact-analysis/artifact-scanning'
            }, {
              title: '开源组件',
              link: '/docs/artifact-analysis/open-source-components'
            }, {
              title: '漏洞库',
              link: '/docs/artifact-analysis/vulnerability-library'
            }, {
              title: '证书库',
              link: '/docs/artifact-analysis/certificate-library'
            },
          ]
        },
        // 统计概览
        {
          title: '统计概览',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '晋级驾驶舱',
              link: '/docs/statistical-overview/promotion-to-the-cockpit'
            }, {
              title: '存储驾驶舱',
              link: '/docs/statistical-overview/storage-cockpit'
            }
          ]
        },
        // 设置管理
        {
          title: '设置管理',
          collapsable: true,
          meta: [],
          children: [
            {
              title: "用户管理",
              link: "/docs/setting-manage/user-management"
            }, {
              title: "用户组管理",
              link: "/docs/setting-manage/user-group-management"
            }, {
              title: "权限管理",
              link: "/docs/setting-manage/permission-management"
            }, {
              title: "访问令牌",
              link: "/docs/setting-manage/access-token"
            }, {
              title: "全局设置",
              link: "/docs/setting-manage/global-settings"
            }, {
              title: "健康监测",
              link: "/docs/setting-manage/health-monitor"
            }, {
              title: "高级运维",
              link: "/docs/setting-manage/senior-ops"
            }
          ]
        },
        // 部署、监控与维护
        {
          title: '部署、监控与维护',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '虚拟机安装',
              link: '/docs/deploy/vm-deploy',
            },
            {
              title: '配置参数详解',
              link: '/docs/deploy/configration-deploy',
            },
            {
              title: 'VM启动命令详解',
              link: '/docs/deploy/vm-comand-deploy'
            },
            {
              title: 'Docker容器安装',
              link: '/docs/deploy/docker-deploy'
            },
            {
              title: 'Docker compose安装',
              link: '/docs/deploy/docker compose-deploy'
            },
            {
              title: '多节点集群配置',
              link: '/docs/deploy/cluster-deploy'
            },
            {
              title: '集群在线维护',
              link: '/docs/deploy/operation'
            }
          ]
        },
        // 工具集成示例
        {
          title: '工具集成示例',
          collapsable: true,
          meta: [],
          children: [
            {
              title: 'Maven',
              link: '/docs/tools/maven',
            },
            {
              title: 'Docker',
              link: '/docs/tools/Docker',
            },
            {
              title: 'Gradle',
              link: '/docs/tools/gradle'
            },
            {
              title: 'Ant/lvy',
              link: '/docs/tools/Ant-Ivy'
            },
            {
              title: 'Yarn/NPM',
              link: '/docs/tools/yarn-npm'
            },
            {
              title: 'Ohpm',
              link: '/docs/tools/ohpm'
            },
            {
              title: 'Go',
              link: '/docs/tools/go'
            },
            {
              title: 'Nuget/Mono',
              link: '/docs/tools/Nuget-Mono'
            },
            {
              title: 'Pypi',
              link: '/docs/tools/Pypi'
            },
            {
              title: 'Conda',
              link: '/docs/tools/Conda'
            },
            {
              title: 'SBT',
              link: '/docs/tools/SBT'
            },
            {
              title: 'Yum/Rpm',
              link: '/docs/tools/Yum-Rpm'
            },
            {
              title: 'Debian',
              link: '/docs/tools/debian'
            },
            {
              title: 'Conan/C&C++',
              link: '/docs/tools/conan-c'
            },
            {
              title: 'Cocoapods',
              link: '/docs/tools/cocoapods'
            },

            {
              title: 'Cargo',
              link: '/docs/tools/Cargo'
            },
            {
              title: 'Huggingface',
              link: '/docs/tools/huggingface'
            },
            {
              title: 'Ollama',
              link: '/docs/tools/ollama'
            },
            {
              title: 'Pub',
              link: '/docs/tools/pub'
            },
            {
              title: 'Helm',
              link: '/docs/tools/helm'
            },
            {
              title: 'GitLFS',
              link: '/docs/tools/gitlfs'
            },

          ]
        },
        // 命令工具说明文档
        {
          title:  '命令工具文档',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '工具概述',
              link: '/docs/command-tool-description/command-tool-description-outline'
            }, {
              title: '操作指南',
              link: '/docs/command-tool-description/command-tool-description-operation'
            }
          ]
        },
      ],
      // Open API
      openApi:[
        // 用户管理
        {
          title: '用户管理相关接口',
          collapsable: true,
          meta: [],
          children: [
            {
              title: 'PUT 新增用户',
              link: '/api/user/create'
            }, {
              title: 'PUT 修改用户',
              link: '/api/user/edit'
            }, {
              title: 'DELETE 删除用户',
              link: '/api/user/delete'
            }, {
              title: 'GET 获取用户信息',
              link: '/api/user/detail'
            }, {
              title: 'POST 获取用户列表',
              link: '/api/user/list'
            },
          ]
        },
        // 用户组管理
        {
          title: '用户组管理相关接口',
          collapsable: true,
          meta: [],
          children: [
            {
              title: 'PUT 新增用户组',
              link: '/api/user-group/create'
            }, {
              title: 'PUT 修改用户组',
              link: '/api/user-group/edit'
            }, {
              title: 'DELETE 删除用户组',
              link: '/api/user-group/delete'
            }, {
              title: 'GET 查询用户组列表',
              link: '/api/user-group/list'
            },
          ]
        },
        // 权限管理
        {
          title: '权限管理相关接口',
          collapsable: true,
          meta: [],
          children: [
            {
              title: 'PUT 新增权限',
              link: '/api/permission/create'
            }, {
              title: 'PUT 修改权限',
              link: '/api/permission/edit'
            }, {
              title: 'DELETE 删除权限',
              link: '/api/permission/delete'
            }, {
              title: 'GET 权限列表',
              link: '/api/permission/list'
            }, {
              title: 'GET 获取权限详情',
              link: '/api/permission/detail'
            },
          ]
        },
        // 存储管理
        {
          title: '存储管理相关接口',
          collapsable: true,
          meta: [],
          children: [
            {
              title: 'PUT 新增存储空间',
              link: '/api/storage/create'
            }, {
              title: 'GET 查询存储空间列表',
              link: '/api/storage/list'
            }, {
              title: 'DELETE 删除存储空间',
              link: '/api/storage/delete'
            }, {
              title: 'PUT 修改存储空间权限',
              link: '/api/storage/edit'
            }, {
              title: 'GET 获取存储空间信息',
              link: '/api/storage/detail'
            },
          ]
        },
        // 仓库管理
        {
          title: '仓库管理相关接口',
          collapsable: true,
          meta: [],
          children: [
            {
              title: 'PUT 新增/修改仓库',
              link: '/api/warehouse/update'
            }, {
              title: 'DELETE 删除仓库',
              link: '/api/warehouse/delete'
            }, {
              title: 'GET 获取仓库信息',
              link: '/api/warehouse/detail'
            }, {
              title: 'PUT 设置联邦仓库',
              link: '/api/warehouse/federal'
            }, {
              title: 'POST 设置仓库权限',
              link: '/api/warehouse/permission'
            }, {
              title: 'GET 浏览仓库内容',
              link: '/api/warehouse/browse-repository'
            }, {
              title: 'GET 查询仓库列表',
              link: '/api/warehouse/list'
            },
          ]
        },
        // 制品管理
        {
          title: '制品管理相关接口',
          collapsable: true,
          meta: [],
          children: [
            {
              title: 'GET 搜索制品',
              link: '/api/artifact/search'
            }, {
              title: 'GET 获取制品信息',
              link: '/api/artifact/detail'
            }, {
              title: 'GET 下载目录',
              link: '/api/artifact/download-catalog'
            }, {
              title: 'GET 下载制品',
              link: '/api/artifact/download'
            }, {
              title: 'DELETE 删除制品',
              link: '/api/artifact/delete'
            }, {
              title: 'GET 查询制品晋级状态',
              link: '/api/artifact/query'
            }, {
              title: 'POST 复制制品',
              link: '/api/artifact/copy'
            }, {
              title: 'POST 移动制品',
              link: '/api/artifact/move'
            }, {
              title: 'POST 晋级制品',
              link: '/api/artifact/qualify'
            }, {
              title: 'POST 上传文件（支持批量）',
              link: '/api/artifact/upload'
            }, {
              title: 'POST 制品批量下载获取路径',
              link: '/api/artifact/batch-download'
            }
          ]
        },
        // 制品元数据
        {
          title: '制品元数据管理相关接口',
          collapsable: true,
          meta: [],
          children: [
            {
              title: 'PUT 新增元数据（单个）',
              link: '/api/metadata/create'
            }, {
              title: 'POST 新增元数据（批量）',
              link: '/api/metadata/batch-create'
            }, {
              title: 'POST 修改元数据',
              link: '/api/metadata/edit'
            }, {
              title: 'POST 删除元数据',
              link: '/api/metadata/delete'
            },
          ]
        },
        // 漏洞管理
        {
          title: '漏洞管理相关接口',
          collapsable: true,
          meta: [],
          children: [
            {
              title: 'GET 仓库漏洞统计',
              link: '/api/vulnerability/statistics'
            }, {
              title: 'GET 获取漏洞分页列表',
              link: '/api/vulnerability/page-list'
            }, {
              title: 'GET 导出漏洞影响范围',
              link: '/api/vulnerability/influence-scope'
            }
          ]
        },
        // 系统设置
        {
          title: '系统设置相关接口',
          collapsable: true,
          meta: [],
          children: [
            {
              title: 'PUT 设置负载端口',
              link: '/api/system-setting/set-port'
            }, {
              title: 'GET 获取负载端口',
              link: '/api/system-setting/get-port'
            }, {
              title: 'PUT 设置负载地址',
              link: '/api/system-setting/set-address'
            }, {
              title: 'GET 获取负载地址',
              link: '/api/system-setting/get-address'
            }, {
              title: 'GET 系统健康检查',
              link: '/api/system-setting/sys-health'
            }, {
              title: 'GET 生成token',
              link: '/api/system-setting/generate-token'
            }
          ]
        },
      ],
      // 常见问题
      QA: [
        // 常见问题
        {
          title: '常见问题',
          collapsable: true,
          meta: [],
          children: [
            {
              title: '最佳实践文档',
              link: '/qa/best-practice'
            }, {
              title: '类型支持文档',
              link: '/qa/type-support'
            }, {
              title: 'WebDAV支持文档',
              link: '/qa/webdav'
            }, {
              title: '同步支持文档',
              link: '/qa/full-download'
            }, {
              title: 'HuggingFace使用文档',
              link: '/qa/huggingface'
            }, {
              title: 'JFrog的接口适配文档',
              link: '/qa/JFrogApi'
            }, {
              title: 'Jenkins插件',
              link: '/qa/jenkins-plugin'
            }, {
              title: 'Harbor同步方法',
              link: '/qa/harbor'
            }
          ]
        }
      ],
    },
    sidebarDepth: 2,
    nextLinks: true,
    prevLinks: true,
  },
  plugins: [
    contentExtractor,
    '@vuepress/back-to-top',
    [require('./plugins/alert'), {}],
    ['@vuepress/plugin-search', {
      maxSuggestions: 10,
    }],
    [
      'vuepress-plugin-code-copy',
      {
        align: 'top', // 复制按钮的位置，可选值 'top', 'bottom', 'bottom-left', 'bottom-right'
        color: '#3eaf7c', // 按钮颜色
        backgroundTransition: true, // 背景过渡效果
        successText: '已复制!', // 复制成功时显示的文本
      },
    ],
  ],
  markdown: {
    lineNumbers: true
  },
  chainWebpack: (config, isServer) => {
    config.resolve.alias.set('vue$', 'vue/dist/vue.esm.js'); // 确保使用完整版本的 Vue
  },
}
