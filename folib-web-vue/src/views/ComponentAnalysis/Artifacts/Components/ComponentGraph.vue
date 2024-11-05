<template>
  <div class="wapper">
    <p>{{ $t('Artifacts.ComponentInformation') }}</p>
    <p>{{ $t('Artifacts.ComponentOverview') }}</p>
    <div class="graph">
      <div id="components" ref="components"></div>
    </div>
  </div>
</template>

<script>
import { formatTimestamp } from "@/utils/util";
import G6 from '@antv/g6';
export default {
  props: {
    artifactData: {
			type: Object,
			default: {},
    },
  },
  watch: {
    artifactData() {
      this.buildData();
    },
    deep: true,
  },
  mounted() {
    this.buildData();
  },

  beforeDestroy: function () {
  },
  data() {
    return {
      components: [],
      componentGraph: null,
    };
  },
  methods: {
    buildData() {
      this.initGraph();
    },
    initGraph() {
      if (this.componentGraph) {
				//如果存在画布，销毁画布，重新渲染
				this.componentGraph.destroy()
			}
      if (!this.artifactData.artifact.componentSet) {
        return
      }
      this.components = this.artifactData.artifact.componentSet
      const {
        Util
      } = G6;
      const tooltip = new G6.Tooltip({
        // offsetX and offsetY include the padding of the parent container
        offsetX: 10,
        offsetY: 10,
        // the types of items that allow the tooltip show up
        // 允许出现 tooltip 的 item 类型
        // custom the tooltip's content
        // 自定义 tooltip 内容
        getContent: (e) => {
          const outDiv = document.createElement('div');
          //outDiv.style.padding = '0px 0px 20px 0px';
          const count = e.item.getModel().count;
          const criticalCount = e.item.getModel().criticalCount;
          const highCount = e.item.getModel().highCount;
          const mediumCount = e.item.getModel().mediumCount;
          const lowCount = e.item.getModel().lowCount;
          outDiv.innerHTML = `<p>漏洞总数 ${count}</p><p>严重数量 ${criticalCount}</p><p>高危数量 ${highCount}</p><p>中危数量 ${mediumCount}</p><p>低危数量 ${lowCount}</p>`;
          return outDiv;
        },
        shouldBegin: (e) => {
          if (e.item.getModel().label && e.item.getModel().id != 0) return true;
          return false;
        },
      });
      const colorArr = [
        '#5B8FF9',
        '#5AD8A6',
        '#5D7092',
        '#F6BD16',
        '#6F5EF9',
        '#6DC8EC',
        '#D3EEF9',
        '#DECFEA',
        '#FFE0C7',
        '#1E9493',
        '#BBDEDE',
        '#FF99C3',
        '#FFE0ED',
        '#CDDDFD',
        '#CDF3E4',
        '#CED4DE',
        '#FCEBB9',
        '#D3CEFD',
        '#945FB9',
        '#FF9845',
      ];
      let artifactName = ""
      if (this.artifactData.layout.toLowerCase() !== "docker") {
          let arr = this.artifactData.artifact.artifactPath.split("/")
          artifactName = arr[arr.length -1]
      } else {
        artifactName = this.artifactData.artifact.artifactCoordinates.imageName
      }
      let components = []
      this.components.forEach(item => {
        components.push({
          label: item.fileName,
          id: item.uuid,
          color: item.vulnerabilitiesCount>0?'#f86c6b':'#5AD8A6',
          count: item.vulnerabilitiesCount,
          criticalCount: item.criticalVulnerabilitiesCount,
          highCount: item.highVulnerabilitiesCount,
          mediumCount: item.mediumVulnerabilitiesCount,
          lowCount: item.lowVulnerabilitiesCount,
        })
      })
      const rawData = {
        label: artifactName,
        id: '0',
        children: components,
      };

      G6.registerNode(
        'dice-mind-map-root', {
        jsx: (cfg) => {
          const width = Util.getTextSize(cfg.label, 16)[0] + 24;
          const stroke = cfg.style.stroke || '#096dd9';
          const fill = cfg.style.fill;

          return `
      <group>
        <rect draggable="true" style={{width: ${width}, height: 42, stroke: ${stroke}, radius: 4}} keyshape>
          <text style={{ fontSize: 16, marginLeft: 12, marginTop: 12 }}>${cfg.label}</text>
          <text style={{ marginLeft: ${width - 16
            }, marginTop: -20, stroke: '#66ccff', fill: '#000', cursor: 'pointer', opacity: ${cfg.hover ? 0.75 : 0
            } }} action="add">+</text>
        </rect>
      </group>
    `;
        },
        getAnchorPoints() {
          return [
            [0, 0.5],
            [1, 0.5],
          ];
        },
      },
        'single-node',
      );
      G6.registerNode(
        'dice-mind-map-sub', {
        jsx: (cfg) => {
          const width = Util.getTextSize(cfg.label, 14)[0] + 24;
          const color = cfg.color || cfg.style.stroke;

          return `
      <group>
        <rect draggable="true" style={{width: ${width + 24}, height: 22}} keyshape>
          <text draggable="true" style={{ fontSize: 14, marginLeft: 12, marginTop: 6 }}>${cfg.label
            }</text>
          <text style={{ marginLeft: ${width - 8
            }, marginTop: -10, stroke: ${color}, fill: '#000', cursor: 'pointer', opacity: ${cfg.hover ? 0.75 : 0
            }, next: 'inline' }} action="add">+</text>
          <text style={{ marginLeft: ${width - 4
            }, marginTop: -10, stroke: ${color}, fill: '#000', cursor: 'pointer', opacity: ${cfg.hover ? 0.75 : 0
            }, next: 'inline' }} action="delete">-</text>
        </rect>
        <rect style={{ fill: ${color}, width: ${width + 24}, height: 2, x: 0, y: 22 }} />

      </group>
    `;
        },
        getAnchorPoints() {
          return [
            [0, 0.965],
            [1, 0.965],
          ];
        },
      },
        'single-node',
      );
      G6.registerNode(
        'dice-mind-map-leaf', {
        jsx: (cfg) => {
          const width = Util.getTextSize(cfg.label, 12)[0] + 24;
          const color = cfg.color || cfg.style.stroke;

          return `
      <group>
        <rect draggable="true" style={{width: ${width + 20}, height: 26, fill: 'transparent' }}>
          <text style={{ fontSize: 12, marginLeft: 12, marginTop: 6 }}>${cfg.label}</text>
              <text style={{ marginLeft: ${width - 8
            }, marginTop: -10, stroke: ${color}, fill: '#000', cursor: 'pointer', opacity: ${cfg.hover ? 0.75 : 0
            }, next: 'inline' }} action="add">+</text>
              <text style={{ marginLeft: ${width - 4
            }, marginTop: -10, stroke: ${color}, fill: '#000', cursor: 'pointer', opacity: ${cfg.hover ? 0.75 : 0
            }, next: 'inline' }} action="delete">-</text>
        </rect>
        <rect style={{ fill: ${color}, width: ${width + 24}, height: 2, x: 0, y: 32 }} />

      </group>
    `;
        },
        getAnchorPoints() {
          return [
            [0, 0.965],
            [1, 0.965],
          ];
        },
      },
        'single-node',
      );

      const dataTransform = (data) => {
        const changeData = (d, level = 0, color) => {
          const data = {
            ...d,
          };
          switch (level) {
            case 0:
              data.type = 'dice-mind-map-root';
              break;
            case 1:
              data.type = 'dice-mind-map-sub';
              break;
            default:
              data.type = 'dice-mind-map-leaf';
              break;
          }

          data.hover = false;

          if (color) {
            data.color = color;
          }

          if (level === 1 && !d.direction) {
            if (!d.direction) {
              data.direction = 'right'
            }
          }

          if (d.children) {
            data.children = d.children.map((child) => changeData(child, level + 1, data.color));
          }
          return data;
        };
        return changeData(data);
      };
			const container = document.getElementById('components');
      const width = container.scrollWidth;
      const height = (container.scrollHeight || 686) - 20;
      this.componentGraph = new G6.TreeGraph({
        container: 'components',
        width,
        height,
        fitView: true,
        fitViewPadding: [20, 20],
        plugins: [tooltip],
        layout: {
          type: 'mindmap',
          direction: 'H',
          getHeight: () => {
            return 16;
          },
          getWidth: (node) => {
            return node.level === 0 ?
              Util.getTextSize(node.label, 16)[0] + 12 :
              Util.getTextSize(node.label, 12)[0];
          },
          getVGap: () => {
            return 10;
          },
          getHGap: () => {
            return 60;
          },
          getSide: (node) => {
            return node.data.direction;
          },
        },
        defaultEdge: {
          type: 'cubic-horizontal',
          style: {
            lineWidth: 2,
          },
        },
        minZoom: 0.48,
        modes: {
          default: ['drag-canvas', 'zoom-canvas'],
        },
      });
      this.componentGraph.data(dataTransform(rawData));
      this.componentGraph.render();
      if (typeof window !== 'undefined')
				window.onresize = () => {
        if (!this.componentGraph || this.componentGraph.get('destroyed')) return;
        if (!container || !container.scrollWidth || !container.scrollHeight) return;
        this.componentGraph.changeSize(container.scrollWidth, container.scrollHeight);
      }
    }
  },
};
</script>

<style lang="scss" scoped>
.wrapper {
  & p:nth-of-type(1) {
    font-size: 20px;
    font-weight: 600;
    color: #17232f;
  }

  & p:nth-of-type(2) {
    font-size: 14px;
    color: #656464;
  }
}
</style>
