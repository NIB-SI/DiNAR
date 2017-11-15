#########################
# .graphML for igraph R #
# zagor 14-06-2016      #
#########################


mydir = dir1
n1 = mydir+'/'+a
e1 = mydir+'/'+b
out = mydir+'/'+c

fN = open (n1, "r")
fR = open (e1, "r")
fG = open (out, "w")


import numpy as np
import datetime

print "vertices: selected column with IDs: 1"
print "vertices: selected column for up/dow regulation: last"
print "edges: selected column for existance of reaction: last"

# vertex values/options
# {'NA': , '0': , '-1': , '1': , '-2': , '2': , '-3': , '3': }
vertexSize = {'NA': 2**(-1*(2**8)), '0.00': 5, '-1.00': 10, '1.00': 10, '-2.00': 15, '2.00': 15, '-3.00': 20, '3.00': 20}
vertexColor = {'NA': '#FFFFFF', '0.00': '#F0F0F0', '-1.00': '#00FF00', '1.00': '#FFFF00', '-2.00': '#0000FF', '2.00': '#FFA500', '-3.00': '#800080', '3.00': '#FF0000'}
vertexFrameColor = {'NA': '#000000', '0.00': 'NA', '-1.00': 'NA', '1.00': 'NA', '-2.00': 'NA', '2.00': 'NA', '-3.00': 'NA', '3.00': 'NA'}
vertexShape = {'NA': 'none', '0.00': 'circle', '-1.00': 'circle', '1.00': 'circle', '-2.00': 'circle', '2.00': 'circle', '-3.00': 'circle', '3.00': 'circle'}
# vertexLabel ... coded later by column, for NA ommited
vertexLabelCex = {'NA': 2**(-1*(2**8)), '0.00': 0.25, '-1.00': 0.50, '1.00': 0.50, '-2.00': 0.75, '2.00': 0.75, '-3.00': 1, '3.00': 1}
vertexLabelColor = {'NA': '#FFFFFF', '0.00': '#FFFFFF', '-1.00': '#000000', '1.00': '#000000', '-2.00': '#000000', '2.00': '#000000', '-3.00': '#000000', '3.00': '#000000'}


# edges values/options
# {0.00: ,0.25: , 0.50: , 0.75: , 1.00: }
edgeColor = {'0.00': '#FFFFFF','0.25': '#C0C0C0', '0.50': '#778899', '0.75': '#696969', '1.00': '#000000'}
edgeWidth = {'0.00': 2**(-1*(2**8)), '0.25': 0.50, '0.50': 0.50, '0.75': 1.00, '1.00': 1.00}
edgeArrowSize = {'0.00': 2**(-1*(2**8)), '0.25': 0.05, '0.50': 0.10, '0.75': 0.10, '1.00': 0.20}
edgeArrowWidth = {'0.00': 2**(-1*(2**8)), '0.25': 0.50, '0.50': 0.50, '0.75': 0.50, '1.00': 0.50}
edgeLty = {'0.00': '' ,'0.25': 'solid', '0.50': 'solid', '0.75': 'solid', '1.00': 'solid'}# twodash for test
# edgeLabel ... coded later by column, for 0 ommited
edgeArrowMode = {'NA': 3, '-': 3, 'binding': 3, 'unk_TF': 3, 'activation': 2, 'act_TF': 2, 'inhibition': 0, 'synthesis': 3, 'inh_TF': 0}
edgeLabelCex = {'0.00': 2**(-1*(2**8)), '0.25': 0.50, '0.50': 0.50, '0.75': 0.75, '1.00': 0.75}

now = datetime.datetime.now()
print "\n"
print now.strftime("%Y-%m-%d %H:%M")
print ("read reactions")

# read reactions
line = fR.readline()
header = line.rstrip()
edgeAttr0 = header.split('\t')
print edgeAttr0
line = fR.readline()
react = []
while line:
    line = line.rstrip()
    tmp = []
    tmp = line.split('\t')
    react.append(tmp)
    line = fR.readline()
fR.close()

# check if number .lstrip('-').replace('.','',1).isdigit()]
# does not work on numpy array
myreact = np.array(react)
for i in range (len(myreact)):
    myreact[i][len(myreact[i])-1] = float(myreact[i][len(myreact[i])-1])
connected = myreact[myreact[:, -1] != '0.0']
connectedVert = sorted(list(set(list(connected[:,0]) + list(connected[:,1]))))

now = datetime.datetime.now()
print "\n"
print now.strftime("%Y-%m-%d %H:%M")
print ("read node descriptions")

# read node descriptions
line = fN.readline()
header = line.rstrip()
vertAttr0 = header.split('\t')
print vertAttr0
line = fN.readline()
nodes = []
conn = []
while line:
    line = line.rstrip()
    tmp = []
    tmp = line.split("\t")
    if tmp[0] in connectedVert:
        conn.append(1)
    else:
        conn.append(0)
    nodes.append(tmp)
    line = fN.readline()  
fN.close()
mynodes = np.array(nodes)


fG.writelines(
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\n"
"    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
"    xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns\n"
"     http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n"
)

cnt = 0
cntN = []
cntN.append(cnt)
for i in vertAttr0:
    fG.write("%s%s%s%s%s\n" % ("  <key attr.name=\"",i,"\" attr.type=\"string\" for=\"node\" id=\"d",cnt,"\"/>"))
    cnt = cnt + 1
    cntN.append(cnt)
############
"""
according to:
vertexSize = {'NA': 30, '0': 6, '-1': 12, '1': 12, '-2': 18, '2': 18, '-3': 24, '3': 24}
vertexColor = {'NA': '#000000', '0': '#F0F0F0', '-1': '#00FF00', '1': '#FFFF00', '-2': '#0000FF', '2': '#FFA500', '-3': '#800080', '3': '#FF0000'}
vertexFrameColor = {'NA': '#000000', '0': 'NA', '-1': 'NA', '1': 'NA', '-2': 'NA', '2': 'NA', '-3': 'NA', '3': 'NA'}
vertexShape = {'NA': 'none', '0': 'circle', '-1': 'circle', '1': 'circle', '-2': 'circle', '2': 'circle', '-3': 'circle', '3': 'circle'}
vertexLabel ... coded later by column, for NA ommited
vertexLabelCex = {'NA': 2, '0': 0.33, '-1': 0.66, '1': 0.66, '-2': 0.99, '2': 0.99, '-3': 1.32, '3': 1.32}
vertexLabelColor = {'NA': '#000000', '0': '#FFFFFF', '-1': '#000000', '1': '#000000', '-2': '#000000', '2': '#000000', '-3': '#000000', '3': '#000000'}
"""
fG.write("%s%s%s\n" % ("  <key for=\"node\" id=\"d",cnt,"\" attr.type=\"float\" attr.name=\"size\">"))#float 50
fG.write("%s\n" % "   <default>15</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"node\" id=\"d",cnt,"\" attr.type=\"string\" attr.name=\"color\">"))
fG.write("%s\n" % "   <default>#FF00FF</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"node\" id=\"d",cnt,"\" attr.type=\"string\" attr.name=\"frame.color\">"))
fG.write("%s\n" % "   <default>#FF00FF</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"node\" id=\"d",cnt,"\" attr.type=\"string\" attr.name=\"shape\">"))#none to hide NA nodes
fG.write("%s\n" % "   <default>none</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"node\" id=\"d",cnt,"\" attr.type=\"string\" attr.name=\"label\">")) # just to check <default>BLA</default>
fG.write("%s\n" % "   <default></default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"node\" id=\"d",cnt,"\" attr.type=\"float\" attr.name=\"label.cex\">"))#float 3
fG.write("%s\n" % "   <default>0.5</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"node\" id=\"d",cnt,"\" attr.type=\"string\" attr.name=\"label.color\">"))
fG.write("%s\n" % "   <default>#00FFFF</default>")
fG.write("  </key>\n")
cnt = cnt + 1
############

cntR = []
cntR.append(cnt)

for i in edgeAttr0[0:(len(edgeAttr0)-1)]:
    fG.write("%s%s%s%s%s\n" % ("  <key attr.name=\"",i,"\" attr.type=\"string\" for=\"edge\" id=\"d",cnt,"\"/>"))
    cnt = cnt + 1
    cntR.append(cnt)
fG.write("%s%s%s%s%s\n" % ("  <key attr.name=\"",edgeAttr0[(len(edgeAttr0)-1)],"\" attr.type=\"string\" for=\"edge\" id=\"d",cnt,"\"/>"))
cnt = cnt + 1
cntR.append(cnt)

############
"""
according to:
edgeColor = {0.00: '#FF00FF ',0.25: '#C0C0C0 ', 0.50: '#778899 ', 0.75: '#696969', 1.00: '#000000'}
edgeWidth = {0.00: 2,0.25: 0.25, 0.50: 0.50, 0.75: 0.75, 1.00: 1.00}
edgeArrowSize = {0.00: 0, 0.25: 0.25, 0.50: 0.50, 0.75: 0.75, 1.00: 1.00}
edgeArrowWidth = {0.00: 0, 0.25: 0.25, 0.50: 0.50, 0.75: 0.75, 1.00: 1.00}
edgeLty = {0.00: '',0.25: 3, 0.50: 3, 0.75: 3, 1.00: 1}
# edgeLabel ... coded later by column, for 0 ommited
edgeArrowMode = {'binding': 3, 'unk_TF': 3, 'activation': 2, 'act_TF': 2, 'inhibition': 0, 'synthesis': 3, 'inh_TF': 0}
"""
fG.write("%s%s%s\n" % ("  <key for=\"edge\" id=\"d",cnt,"\" attr.type=\"string\" attr.name=\"color\">"))
fG.write("%s\n" % "   <default>#FF00FF</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"edge\" id=\"d",cnt,"\" attr.type=\"float\" attr.name=\"width\">"))#float 3
fG.write("%s\n" % "   <default>1</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"edge\" id=\"d",cnt,"\" attr.type=\"float\" attr.name=\"arrow.size\">"))#float 3
fG.write("%s\n" % "   <default>1</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"edge\" id=\"d",cnt,"\" attr.type=\"float\" attr.name=\"arrow.width\">"))#float 3
fG.write("%s\n" % "   <default>1</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"edge\" id=\"d",cnt,"\" attr.type=\"string\" attr.name=\"lty\">"))
fG.write("%s\n" % "   <default>twodash</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"edge\" id=\"d",cnt,"\" attr.type=\"string\" attr.name=\"label\">")) # for debug write GRR
fG.write("%s\n" % "   <default></default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"edge\" id=\"d",cnt,"\" attr.type=\"int\" attr.name=\"arrow.mode\">"))#int 1
fG.write("%s\n" % "   <default>3</default>")
fG.write("  </key>\n")
cnt = cnt + 1
fG.write("%s%s%s\n" % ("  <key for=\"edge\" id=\"d",cnt,"\" attr.type=\"float\" attr.name=\"label.cex\">"))#int
fG.write("%s\n" % "   <default>2</default>")
fG.write("  </key>\n")
cnt = cnt + 1
############
cntG = cnt
fG.write("  <graph edgedefault=\"directed\" id=\"G\">\n")

# HINTS!!!
# lty blank == no edges
# shape 'none'== no vertices

# create NULL matrix
# http://docs.scipy.org/doc/numpy/reference/generated/numpy.chararray.html
"""
Starting from numpy 1.4, if one needs arrays of strings, it is recommended to use arrays of
dtype object_, string_ or unicode_,
and use the free functions in the numpy.char module for fast vectorized string operations.
"""

now = datetime.datetime.now()
print "\n"
print now.strftime("%Y-%m-%d %H:%M")
print ("translateNodes")

translateNodes = np.zeros(shape=(len(mynodes),2),dtype='|S161')
start = cntN[len(cntN)-1] + 0
end = cntR[0]
print "---"
for i in range (len(mynodes)):
    # print "i=",i
    fG.write("%s%s%s\n" % ("    <node id=\"n",i,"\">"))
    translateNodes[i][0] = mynodes[i,0]
    # print translateNodes[i][0]
    # print mynodes[i,0]
    translateNodes[i][1] = 'n' + str(i)
    cntCol = 0
    for j in cntN[0:(len(cntN)-1)]:
        #print "j=",j
        #print "cntCol=", cntCol
        fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",j,"\"><![CDATA[",mynodes[i,cntCol],"]]></data>"))
        cntCol = cntCol + 1
    """
    vertexSize
    vertexColor
    vertexFrameColor
    vertexShape
    # vertexLabel
    vertexLabelCex
    vertexLabelColor
    """
    if (mynodes[i,-1] != 'NA'): # hide NA nodes
        mynodes[i,-1] = format(float(mynodes[i,-1]), '.2f') # convert to float with two decimal points and back to string
        #print (mynodes[i,-1])
        for k in range (start,end):
                if (k==(start+0)):
                    fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexSize.get(mynodes[i,-1]),"</data>"))
                elif (k==(start+1)):
                    fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexColor.get(mynodes[i,-1]),"</data>"))
                elif (k==(start+2)):
                    fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexFrameColor.get(mynodes[i,-1]),"</data>"))
                elif (k==(start+3)):
                    fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexShape.get(mynodes[i,-1]),"</data>"))
                elif (k==(start+4)):
                    fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",mynodes[i,1],"</data>")) # mynodes[i,13]
                elif (k==(start+5)):
                    fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexLabelCex.get(mynodes[i,-1]),"</data>"))
                elif (k==(start+6)):
                    fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexLabelColor.get(mynodes[i,-1]),"</data>"))
                #else:
                #    print "???"
                #    fG.write("%s%s%s\n" % ("      <data key=\"d",k,"\"></data>"))
    else:
        # print mynodes[i,0], mynodes[i,13], mynodes[i,-1]
        for k in range (start,end):
            if (k==(start+0)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexSize.get(mynodes[i,-1]),"</data>"))
            elif (k==(start+1)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexColor.get(mynodes[i,-1]),"</data>"))
            elif (k==(start+2)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexFrameColor.get(mynodes[i,-1]),"</data>"))
            elif (k==(start+3)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexShape.get(mynodes[i,-1]),"</data>"))
            elif (k==(start+4)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",'',"</data>")) # mynodes[i,1]
            elif (k==(start+5)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexLabelCex.get(mynodes[i,-1]),"</data>"))
            elif (k==(start+6)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",vertexLabelColor.get(mynodes[i,-1]),"</data>"))
    fG.write("    </node>\n")
# print "translateNodes"
# print translateNodes

now = datetime.datetime.now()
print "\n"
print now.strftime("%Y-%m-%d %H:%M")
print ("translateEdges")
    
start = cntR[len(cntR)-1] + 0
end = start + 8
myedges = np.array(react)
print "---"
for i in range (len(myedges)):
    i1 = translateNodes[:,0].tolist().index(str(myedges[i,0]))
    i2 = translateNodes[:,0].tolist().index(str(myedges[i,1]))
    #print i1, i2
    fG.write("%s%s%s%s%s%s%s\n" % ("    <edge id=\"e",i,"\" source=\"",translateNodes[i1,1],"\" target=\"",translateNodes[i2,1],"\">"))
    cntCol = 0
    for j in cntR[0:len(cntR)-1]:
        # print cntCol, j
        if (j != cntR[len(cntR)-2]):
            fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",j,"\"><![CDATA[",myedges[i,cntCol],"]]></data>"))
        else:
            edgeStrength = (float(myedges[i,cntCol]))
            # print edgeStrength
            fG.write("%s%s%s%f%s\n" % ("      <data key=\"d",j,"\"><![CDATA[",edgeStrength,"]]></data>"))
        cntCol = cntCol + 1
    """
    edgeColor
    edgeWidth
    edgeArrowSize
    edgeArrowWidth
    edgeLty
    # edgeLabel ... coded later by column, for 0 ommited
    edgeArrowMode
    edgeLabelCex
    """
    myedges[i,-1] = format(float(myedges[i,-1]), '.2f') # convert to float with two decimal points and back to string
    # print (myedges[i,-1]), myedges[i,-1] != '0.00'
    if (myedges[i,-1] != '0.00'): # hide NA-NA a.k.a. 0 edges
        #print (myedges[i,-1])
        for k in range (start,end):
            if (k==(start+0)):
                #fG.write("%s%s%s\n" % ("      <data key=\"d",k,"\"></data>"))
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeColor.get(myedges[i,-1]),"</data>"))
            elif (k==(start+1)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeWidth.get(myedges[i,-1]),"</data>"))
            elif (k==(start+2)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeArrowSize.get(myedges[i,-1]),"</data>"))
            elif (k==(start+3)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeArrowWidth.get(myedges[i,-1]),"</data>"))
            elif (k==(start+4)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeLty.get(myedges[i,-1]),"</data>"))
            elif (k==(start+5)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",myedges[i,2],"</data>"))
            elif (k==(start+6)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeArrowMode.get(myedges[i,2]),"</data>"))
            elif (k==(start+7)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeLabelCex.get(myedges[i,-1]),"</data>"))
                #else:
                #    print "???"
                #    fG.write("%s%s%s\n" % ("      <data key=\"d",k,"\"></data>"))
    else:
        # print myedges[i,0], myedges[i,1], myedges[i,-1]
        for k in range (start,end):
            if (k==(start+0)):
                #fG.write("%s%s%s\n" % ("      <data key=\"d",k,"\"></data>"))
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeColor.get(myedges[i,-1]),"</data>"))
            elif (k==(start+1)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeWidth.get(myedges[i,-1]),"</data>"))
            elif (k==(start+2)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeArrowSize.get(myedges[i,-1]),"</data>"))
            elif (k==(start+3)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeArrowWidth.get(myedges[i,-1]),"</data>"))
            elif (k==(start+4)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",'',"</data>"))
            elif (k==(start+5)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",'',"</data>"))
            elif (k==(start+6)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeArrowMode.get(myedges[i,2]),"</data>"))
            elif (k==(start+7)):
                fG.write("%s%s%s%s%s\n" % ("      <data key=\"d",k,"\">",edgeLabelCex.get(myedges[i,-1]),"</data>"))
    fG.writelines("    </edge>\n")
#########

fG.write("  </graph>\n")

fG.writelines("</graphml>\n")    

fG.close()

now = datetime.datetime.now()
print "\n"
print now.strftime("%Y-%m-%d %H:%M")
print ("finitto")
