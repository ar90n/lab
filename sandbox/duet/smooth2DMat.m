function smat = smooth2DMat(mat,ker)

    if prod(size(ker)) == 1
           kmat = ones(ker,ker)/ker^2;
    else
           kmat = ker;
    end
    
    [kr kc] = size(kmat);
    
    if rem(kr,2) == 0
        kmat = conv2(kmat,ones(2,1)) / 2;
        kr = kr + 1;
    end
    
    if rem(kc,2) == 0
        kmat =conv2(kmat,ones(1,2)) / 2;
        kc = kc + 1;
    end
        
    smat = conv2(expand2DMat(mat,kr,kc),flipud(fliplr(kmat)),'valid');
end
           